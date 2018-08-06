package swaggins.generator.convert

import cats.data
import cats.data.NonEmptyList
import io.scalaland.chimney.dsl._
import swaggins.openapi.model.components.SchemaName
import swaggins.openapi.model.shared.ReferenceRef.ComponentRef
import swaggins.openapi.model.shared._
import swaggins.scala.ast.model
import swaggins.scala.ast.model.{Discriminator => _, _}
import swaggins.scala.ast.ref._

object Converters {

  private val refToTypeRef: ReferenceRef => TypeReference = {
    case ComponentRef(name) => name.transformInto[OrdinaryType]
  }

  def convertSchemaOrRef(
    schemaName: SchemaName,
    schemaOrRef: Reference.Able[Schema]): NonEmptyList[ScalaModel] =
    schemaOrRef match {
      case Right(schema) =>
        convertSchema(schemaName.transformInto[TypeName], schema)
    }

  private def convertDiscriminator(
    discriminator: Discriminator): model.Discriminator = {
    model.Discriminator(
      discriminator.propertyName.map(_.transformInto[FieldName]),
      discriminator.mapping.map(_.map(_.transformInto[OrdinaryType])))
  }

  /**
    * Converts an OpenAPI Schema to a Scala model (e.g. a case class or an ADT).
    * */
  private def convertSchema(typeName: TypeName,
                            schema: Schema): NonEmptyList[ScalaModel] = {
    schema match {
      case ObjectSchema(required, properties) =>
        val fields = properties.map { prop =>
          val isRequired = required.exists(_.apply(prop.name))

          CaseClassField(isRequired,
                         prop.name.transformInto[FieldName],
                         refSchemaToType(prop.schema))
        }

        data.NonEmptyList.one(CaseClass(typeName, fields, None))

      case NumberSchema(None) =>
        data.NonEmptyList.one(ValueClass(typeName, Primitive.Double))

      case StringSchema(None) =>
        data.NonEmptyList.one(ValueClass(typeName, Primitive.String))

      case comp: CompositeSchema =>
        data.NonEmptyList.one(convertCompositeSchema(typeName, comp))
    }
  }

  private def convertCompositeSchema(
    name: TypeName,
    compositeSchema: CompositeSchema): ScalaModel = compositeSchema.kind match {
    case CompositeSchemaKind.OneOf | CompositeSchemaKind.AnyOf =>
      SealedTraitHierarchy(
        name,
        compositeSchema.schemas.flatMap { schemaOrRef =>
          convertSchemaOrRef(SchemaName("SYNTHETIC_NAME"), schemaOrRef)
        },
        compositeSchema.discriminator.map(convertDiscriminator)
      )
  }

  private def refSchemaToType(schema: Reference.Able[Schema]): TypeReference =
    schema match {
      case Left(ref)                 => refToTypeRef(ref.`$ref`)
      case Right(NumberSchema(None)) => Primitive.Double
      case Right(StringSchema(None)) => Primitive.String
      case Right(ArraySchema(items)) => ListType(refSchemaToType(items))
    }
}
