package swaggins.generator.convert

import cats.{~>, Monad}
import cats.data._
import cats.implicits._
import cats.mtl.{ApplicativeAsk, ApplicativeLocal}
import io.scalaland.chimney.dsl._
import swaggins.openapi.model.components.SchemaName
import swaggins.openapi.model.shared.ReferenceRef.ComponentRef
import swaggins.openapi.model.shared._
import swaggins.scala.ast.model
import swaggins.scala.ast.model.{Discriminator => _, _}
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
  ): F[ScalaModel]
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
      def convertSchema(typeName: TypeName, schema: Schema): F[ScalaModel] = {
        schema match {
          case ObjectSchema(required, properties) =>
            val fieldsWithModels = properties.map { prop =>
              val isRequired = required.exists(_.apply(prop.name))

              val (tpe, modelOpt) = refSchemaToType(prop.name, prop.schema)

              (
                CaseClassField(
                  isRequired,
                  prop.name.transformInto[FieldName],
                  tpe
                ),
                modelOpt
              )
            }

            val fields = fieldsWithModels.map(_._1)
            val models = fieldsWithModels.toList.flatMap(_._2)

            val companion = models.toNel.map(CompanionObject(typeName, _))

            CaseClass(typeName, fields, ExtendsClause.empty, companion)
              .pure[F]
              .widen

          case NumberSchema(None) =>
            ValueClass(typeName, Primitive.Double).pure[F].widen

          case StringSchema(None) =>
            ValueClass(typeName, Primitive.String).pure[F].widen

          case comp: CompositeSchema =>
            convertCompositeSchema(typeName, comp)
        }
      }

      def convertCompositeSchema(
        compositeName: TypeName,
        compositeSchema: CompositeSchema
      ): F[ScalaModel] =
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
                StateT
                  .liftF(convertSchemaOrRef(derivedName, schemaOrRef))
                  .map(
                    ScalaModel.extendsClause.set(
                      ExtendsClause(List(OrdinaryType(compositeName.value)))
                    )
                  )
              }
            }

            schemaz
              .runA(1)
              .map { a =>
                SealedTraitHierarchy(
                  compositeName,
                  a,
                  compositeSchema.discriminator.map(convertDiscriminator)
                )
              }
              .widen
        }

      def convertSchemaOrRef(
        schemaName: SchemaName,
        schemaOrRef: Reference.Able[Schema]
      ): F[ScalaModel] =
        schemaOrRef match {
          case Right(schema) =>
            convertSchema(TypeName.parse(schemaName.value), schema)
          case Left(ref) =>
            CaseClass(
              TypeName.parse(schemaName.value),
              NonEmptyList.one(
                CaseClassField(
                  required = true,
                  FieldName("value"),
                  refToTypeRef(ref.`$ref`)
                )
              ),
              ExtendsClause.empty
            ).pure[F].widen
        }
    }

  private def convertDiscriminator(
    discriminator: Discriminator
  ): model.Discriminator = {
    model.Discriminator(
      discriminator.propertyName.map(_.transformInto[FieldName]),
      discriminator.mapping.map(_.map(_.transformInto[OrdinaryType]))
    )
  }

  /**
    * Resolves a schema/reference to a type reference and (optionally) a synthetic type.
    * */
  private def refSchemaToType(
    name: SchemaName,
    schema: Reference.Able[Schema]
  ): (TypeReference, Option[ScalaModel]) =
    schema match {
      case Left(ref)                 => (refToTypeRef(ref.`$ref`), None)
      case Right(NumberSchema(None)) => (Primitive.Double, None)
      case Right(StringSchema(None)) => (Primitive.String, None)
      case Right(StringSchema(Some(values))) =>
        val enumModel = Some(
          Enumerated(
            TypeName.parse(name.value),
            Primitive.String,
            values.map(ScalaLiteral.string(_))
          )
        )

        (name.transformInto[OrdinaryType], enumModel)

      case Right(ArraySchema(items)) =>
        val (childType, childModel) = refSchemaToType(name, items)
        (ListType(childType), childModel)
    }
}
