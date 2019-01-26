package swaggins.generator.convert

import cats.{Applicative, Monad}
import cats.data._
import cats.implicits._
import swaggins.openapi.model.components.SchemaName
import swaggins.openapi.model.shared.ReferenceRef.ComponentRef
import swaggins.openapi.model.shared._
import swaggins.scala.ast.model._
import swaggins.scala.ast.model.body.Body
import swaggins.scala.ast.model.klass.{ClassField, FieldName}
import swaggins.scala.ast.model.values.ScalaLiteral
import swaggins.scala.ast.packages.{PackageName, Packages}
import swaggins.scala.ast.ref._
import swaggins.core.implicits._

trait Converters[F[_]] {

  def convertSchemaOrRef(
    schemaName: SchemaName,
    schemaOrRef: Reference.Able[Schema]
  ): F[ModelWithCompanion]
}

object Converters {
  def apply[F[_]](implicit F: Converters[F]): Converters[F] = F

  private val refToTypeRef: ReferenceRef => TypeReference = {
    case ComponentRef(name) => OrdinaryType(name.value.toCamelCase)
  }

  //todo un-implicify
  implicit def make[F[_]: Packages.Local: Monad]: Converters[F] =
    new Converters[F] {

      /**
        * Converts an OpenAPI Schema to a Scala model (e.g. a case class or an ADT).
        * */
      private def convertSchema(typeName: TypeName,
                                schema: Schema): F[ModelWithCompanion] = {
        schema match {
          case ObjectSchema(required, properties) =>
            properties.traverse { prop =>
              val isRequired = required.exists(_.apply(prop.name))

              Packages.Local
                .local(_.added(PackageName(typeName.value))) {
                  refSchemaToType[F](prop.name, prop.schema)
                }
                .map {
                  case (tpe, modelOpt) =>
                    val field =
                      if (isRequired)
                        ClassField(FieldName(prop.name.value), tpe)
                      else ClassField.optional(FieldName(prop.name.value), tpe)

                    (field, modelOpt)
                }
            }.map { fieldsWithModels =>
              val fields: NonEmptyList[ClassField] = fieldsWithModels.map(_._1)

              val models: Option[NonEmptyList[ModelWithCompanion]] =
                fieldsWithModels.toList.mapFilter(_._2).toNel

              val companion: Option[ScalaModel] = models.map(
                models =>
                  ScalaModel.companionObject(
                    typeName,
                    Body.models(models.flatMap(_.asNel).toList)))

              ModelWithCompanion(ScalaModel.finalCaseClass(typeName,
                                                           fields,
                                                           ExtendsClause.empty),
                                 companion)
            }

          case NumberSchema(None) =>
            ModelWithCompanion
              .justClass(ScalaModel.valueClass(typeName, Primitive.double))
              .pure[F]

          case StringSchema(None) =>
            ModelWithCompanion
              .justClass(ScalaModel.valueClass(typeName, Primitive.string))
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
                  SchemaName(Primitive.string.show).pure[S]
                case Right(NumberSchema(None)) =>
                  SchemaName(Primitive.double.show).pure[S]
                case Right(_) =>
                  getAndIncSyntheticNumber.map(
                    num => SchemaName(s"Anonymous$$$num")
                  )
              }

              derivedWrappedName.flatMap { derivedName =>
                StateT.liftF(convertSchemaOrRef(derivedName, schemaOrRef)).map {
                  ModelWithCompanion.klassExtendsClause.set(
                    ExtendsClause(
                      List(OrdinaryType(compositeName.value.toCamelCase)))
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
  private def refSchemaToType[F[_]: Packages.Ask: Applicative](
    name: SchemaName,
    schema: Reference.Able[Schema]
  ): F[(TypeReference, Option[ModelWithCompanion])] = {
    schema match {
      case Left(ref)                 => (refToTypeRef(ref.`$ref`), None).pure[F].widen
      case Right(NumberSchema(None)) => (Primitive.double, None).pure[F].widen
      case Right(StringSchema(None)) => (Primitive.string, None).pure[F].widen
      case Right(StringSchema(Some(values))) =>
        val enumModel =
          ScalaModel.enumeration(
            TypeName.parse(name.value),
            Primitive.string,
            values.map(ScalaLiteral.string(_))
          )

        TypeReference
          .inPackage(OrdinaryType(name.value.toCamelCase))
          .map(_ -> Some(enumModel))

      case Right(ArraySchema(items)) =>
        refSchemaToType(name, items).map {
          case (childType, childModel) =>
            (TypeReference.listOf(childType), childModel)
        }
    }
  }
}
