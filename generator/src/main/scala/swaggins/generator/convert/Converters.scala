package swaggins.generator.convert

import cats.{~>, Monad}
import cats.data._
import cats.implicits._
import cats.mtl.{ApplicativeAsk, ApplicativeLocal}
import io.scalaland.chimney.dsl._
import swaggins.openapi.model.components.SchemaName
import swaggins.openapi.model.shared.ReferenceRef.ComponentRef
import swaggins.openapi.model.shared._
import swaggins.scala.ast.model._
import swaggins.scala.ast.ref._

case class PackageName(value: String) extends AnyVal
case class Packages(value: List[PackageName]) {
  def added(pkg: PackageName): Packages = copy(pkg :: value)
}

object Packages {
  type Ask[F[_]] = ApplicativeAsk[F, Packages]
  def Ask[F[_]](implicit F: Ask[F]): Ask[F] = F

  type Local[F[_]] = ApplicativeLocal[F, Packages]
  def Local[F[_]](implicit F: Local[F]): Local[F] = F

  val empty: Packages = Packages(Nil)

  def added[F[_]: Local, A](name: PackageName): F ~> F =
    λ[F ~> F](Local[F].local(_.added(name))(_))
}

trait Converters[F[_]] {

  def convertSchemaOrRef(
    schemaName: SchemaName,
    schemaOrRef: Reference.Able[Schema]
  ): F[ModelWithCompanion]
}

object Converters {
  def apply[F[_]](implicit F: Converters[F]): Converters[F] = F

  private val refToTypeRef: ReferenceRef => TypeReference = {
    case ComponentRef(name) => name.transformInto[OrdinaryType]
  }

  //todo un-implicify
  implicit def make[F[_]: Packages.Ask: Monad]: Converters[F] =
    new Converters[F] {

      /**
        * Converts an OpenAPI Schema to a Scala model (e.g. a case class or an ADT).
        * */
      private def convertSchema(typeName: TypeName,
                                schema: Schema): F[ModelWithCompanion] = {
        schema match {
          case ObjectSchema(required, properties) =>
            val fieldsWithModels = properties.map { prop =>
              val isRequired = required.exists(_.apply(prop.name))

              val (tpe, modelOpt) = refSchemaToType(prop.name, prop.schema)

              val field =
                if (isRequired) ClassField(FieldName(prop.name.value), tpe)
                else ClassField.optional(FieldName(prop.name.value), tpe)

              (field, modelOpt)
            }

            val fields: NonEmptyList[ClassField] = fieldsWithModels.map(_._1)

            val models: Option[NonEmptyList[ModelWithCompanion]] =
              fieldsWithModels.toList.mapFilter(_._2).toNel

            val companion: Option[ScalaModel] = models.map(
              models =>
                SingletonObject(Modifiers.empty,
                                typeName,
                                ExtendsClause.empty,
                                Body.models(models.flatMap(_.asNel).toList)))

            ModelWithCompanion(
              ScalaModel.finalCaseClass(typeName, fields, ExtendsClause.empty),
              companion).pure[F]

          case NumberSchema(None) =>
            ModelWithCompanion
              .justClass(ScalaModel.valueClass(typeName, Primitive.Double))
              .pure[F]

          case StringSchema(None) =>
            ModelWithCompanion
              .justClass(ScalaModel.valueClass(typeName, Primitive.String))
              .pure[F]

          case comp: CompositeSchema =>
            convertCompositeSchema(typeName, comp)
        }
      }

      def convertCompositeSchema(
        compositeName: TypeName,
        compositeSchema: CompositeSchema
      ): F[ModelWithCompanion] =
        compositeSchema.kind match {
          case CompositeSchemaKind.OneOf | CompositeSchemaKind.AnyOf =>
            type S[A] = StateT[F, Int, A]

            val schemaz = compositeSchema.schemas.traverse { schemaOrRef =>
              val getAndIncSyntheticNumber: S[Int] = StateT
                .get[F, Int] <* StateT.modify(_ + 1)

              val derivedWrappedName: S[SchemaName] = schemaOrRef match {
                case Left(ref) =>
                  SchemaName(refToTypeRef(ref.`$ref`).show).pure[S]
                case Right(StringSchema(None)) =>
                  SchemaName(Primitive.String.show).pure[S]
                case Right(NumberSchema(None)) =>
                  SchemaName(Primitive.Double.show).pure[S]
                case Right(_) =>
                  getAndIncSyntheticNumber.map(
                    num => SchemaName(s"Anonymous$$$num")
                  )
              }

              derivedWrappedName.flatMap { derivedName =>
                StateT.liftF(convertSchemaOrRef(derivedName, schemaOrRef)).map {
                  ModelWithCompanion.klassExtendsClause.set(
                    ExtendsClause(List(OrdinaryType(compositeName.value)))
                  )
                }
              }
            }

            schemaz.runA(1).map { leafNodeModels =>
              ScalaModel.sealedTraitHierarchy(
                compositeName,
                leafNodeModels.flatMap(_.asNel)
              )
            }
        }

      override def convertSchemaOrRef(
        schemaName: SchemaName,
        schemaOrRef: Reference.Able[Schema]
      ): F[ModelWithCompanion] =
        schemaOrRef match {
          case Right(schema) =>
            convertSchema(TypeName.parse(schemaName.value), schema)
          case Left(ref) =>
            val klazz = ScalaModel.finalCaseClass(
              TypeName.parse(schemaName.value),
              NonEmptyList.one(
                ClassField(
                  FieldName("value"),
                  refToTypeRef(ref.`$ref`)
                )
              ),
              ExtendsClause.empty
            )

            ModelWithCompanion.justClass(klazz).pure[F]
        }
    }

  /**
    * Resolves a schema/reference to a type reference and (optionally) a synthetic type.
    * */
  private def refSchemaToType(
    name: SchemaName,
    schema: Reference.Able[Schema]
  ): (TypeReference, Option[ModelWithCompanion]) =
    schema match {
      case Left(ref)                 => (refToTypeRef(ref.`$ref`), None)
      case Right(NumberSchema(None)) => (Primitive.Double, None)
      case Right(StringSchema(None)) => (Primitive.String, None)
      case Right(StringSchema(Some(values))) =>
        val enumModel =
          ScalaModel.enumeration(
            TypeName.parse(name.value),
            Primitive.String,
            values.map(ScalaLiteral.string(_))
          )

        (name.transformInto[OrdinaryType], Some(enumModel))

      case Right(ArraySchema(items)) =>
        val (childType, childModel) = refSchemaToType(name, items)
        (TypeReference.listOf(childType), childModel)
    }
}
