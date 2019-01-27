package swaggins.generator.convert

import cats.data._
import cats.implicits._
import cats.{Applicative, Monad}
import swaggins.core.implicits._
import swaggins.openapi.model.components.SchemaName
import swaggins.openapi.model.shared.ReferenceRef.ComponentRef
import swaggins.openapi.model.shared._
import swaggins.scala.ast.model._
import swaggins.scala.ast.model.body.Body
import swaggins.scala.ast.model.klass.{ClassField, FieldName}
import swaggins.scala.ast.model.values.ScalaLiteral
import swaggins.scala.ast.packages.{PackageName, Packages}
import swaggins.scala.ast.ref._

trait Converters[F[_]] {

  def convertSchemaOrRef(
    schemaName: SchemaName,
    schemaOrRef: RefOrSchema
  ): F[ModelWithCompanion]
}

object Converters {
  def apply[F[_]](implicit F: Converters[F]): Converters[F] = F

  //todo un-implicify
  implicit def make[F[_]: Packages.Local: Monad]: Converters[F] =
    new ConvertersImpl[F]
}

class ConvertersImpl[F[_]: Packages.Local: Monad] extends Converters[F] {
  override def convertSchemaOrRef(
    schemaName: SchemaName,
    schemaOrRef: RefOrSchema
  ): F[ModelWithCompanion] =
    schemaOrRef match {
      case RefOrSchema.InlineSchema(schema) =>
        convertSchema(TypeName.parse(schemaName.value), schema)
      case RefOrSchema.Reference(ref) =>
        val klazz = ScalaModel.finalCaseClass(
          TypeName.parse(schemaName.value),
          NonEmptyList.one(
            ClassField(
              FieldName("value"),
              refToTypeRef(ref)
            )
          ),
          ExtendsClause.empty
        )

        ModelWithCompanion.justClass(klazz).pure[F]
    }

  private val refToTypeRef: ReferenceRef => TypeReference = {
    case ComponentRef(name) => OrdinaryType(name.value.toCamelCase)
  }

  /**
    * Converts an OpenAPI Schema to a Scala model (e.g. a case class or an ADT).
    * */
  private def convertSchema(typeName: TypeName,
                            schema: Schema): F[ModelWithCompanion] = {
    schema match {
      case obj: ObjectSchema =>
        convertObjectSchema(typeName, obj)

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

  private def convertCompositeSchema(
    compositeName: TypeName,
    compositeSchema: CompositeSchema
  ): F[ModelWithCompanion] = {
    val extendCompositeSchemaRoot =
      ModelWithCompanion.klassExtendsClause.set(
        ExtendsClause(List(OrdinaryType(compositeName.value.toCamelCase)))
      )

    compositeSchema.kind match {
      case CompositeSchemaKind.OneOf | CompositeSchemaKind.AnyOf =>
        type S[A] = StateT[F, Int, A]

        val getAndIncSyntheticNumber: S[Int] = StateT.get[F, Int] <* StateT
          .modify(_ + 1)

        val schemaz: F[NonEmptyList[ModelWithCompanion]] =
          compositeSchema.schemas.nonEmptyTraverse { schemaOrRef =>
            val derivedWrappedName: S[SchemaName] = schemaOrRef match {
              case RefOrSchema.Reference(ref) =>
                SchemaName(refToTypeRef(ref).show).pure[S]
              case RefOrSchema.InlineSchema(StringSchema(None)) =>
                SchemaName(Primitive.string.show).pure[S]
              case RefOrSchema.InlineSchema(NumberSchema(None)) =>
                SchemaName(Primitive.double.show).pure[S]
              case RefOrSchema.InlineSchema(_) =>
                getAndIncSyntheticNumber.map(
                  num => SchemaName(s"Anonymous$$$num")
                )
            }

            derivedWrappedName.flatMapF(convertSchemaOrRef(_, schemaOrRef))
          }.runA(1)

        schemaz.map { leafNodeModels =>
          ScalaModel.sealedTraitHierarchy(
            compositeName,
            leafNodeModels.map(extendCompositeSchemaRoot).flatMap(_.asNel)
          )
        }
    }
  }

  /**
    * Generates models for an object schema.
    * */
  private def convertObjectSchema(typeName: TypeName,
                                  schema: ObjectSchema): F[ModelWithCompanion] =
    Packages.Local.local(_.added(PackageName(typeName.value))) {

      schema.properties.nonEmptyTraverse { prop =>
        val isRequired = schema.required.exists(_.apply(prop.name))
        propertyToFieldWithModel(isRequired, prop)

      }.map { fieldsWithModels =>
        val fields: NonEmptyList[ClassField] = fieldsWithModels.map(_._1)

        val models: Option[NonEmptyList[ModelWithCompanion]] =
          fieldsWithModels.toList.mapFilter(_._2).toNel

        val companion: Option[ScalaModel] = models.map(
          models =>
            ScalaModel.companionObject(
              typeName,
              Body.models(models.flatMap(_.asNel).toList)))

        ModelWithCompanion(
          ScalaModel.finalCaseClass(typeName, fields, ExtendsClause.empty),
          companion)
      }
    }

  /**
    * Generates a class field and, if the type is anonymous, its definition.
    * */
  def propertyToFieldWithModel(
    isRequired: Boolean,
    prop: Property): F[(ClassField, Option[ModelWithCompanion])] = {

    refSchemaToType(prop.name, prop.schema).map {
      case (tpe, modelOpt) =>
        val checkedType =
          if (isRequired) tpe else TypeReference.optional(tpe)

        val field =
          ClassField(FieldName(prop.name.value), checkedType)

        (field, modelOpt)
    }
  }

  /**
    * Resolves a schema name and schema/reference to a type reference
    * (to be used as a class field) and (possibly) a synthetic type.
    * If a synthetic type is created, its name is based on the property's name,
    * and the reference is qualified with the current package scope.
    * */
  private def refSchemaToType(
    name: SchemaName,
    schema: RefOrSchema
  ): F[(TypeReference, Option[ModelWithCompanion])] = {
    schema match {
      case RefOrSchema.Reference(ref) => (refToTypeRef(ref), None).pure[F].widen
      case RefOrSchema.InlineSchema(NumberSchema(None)) =>
        (Primitive.double, None).pure[F].widen
      case RefOrSchema.InlineSchema(StringSchema(None)) =>
        (Primitive.string, None).pure[F].widen
      case RefOrSchema.InlineSchema(StringSchema(Some(values))) =>
        val enumModel =
          ScalaModel.enumeration(
            TypeName.parse(name.value),
            Primitive.string,
            values.map(ScalaLiteral.string(_))
          )

        TypeReference
          .inPackage(OrdinaryType(name.value.toCamelCase))
          .map(_ -> Some(enumModel))

      case RefOrSchema.InlineSchema(ArraySchema(items)) =>
        refSchemaToType(name, items).map {
          case (childType, childModel) =>
            (TypeReference.listOf(childType), childModel)
        }
    }
  }
}
