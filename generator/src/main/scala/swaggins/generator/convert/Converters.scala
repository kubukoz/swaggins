package swaggins.generator.convert

import cats.data.{NonEmptyList, State}
import cats.implicits._
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
    schemaOrRef: Reference.Able[Schema]): ScalaModel =
    schemaOrRef match {
      case Right(schema) =>
        convertSchema(TypeName.parse(schemaName.value), schema)
      case Left(ref) =>
        val alias = CaseClass(TypeName.parse(schemaName.value),
                              NonEmptyList.one(
                                CaseClassField(required = true,
                                               FieldName("value"),
                                               refToTypeRef(ref.`$ref`))),
                              ExtendsClause.empty)

        alias
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
                            schema: Schema): ScalaModel = {
    schema match {
      case ObjectSchema(required, properties) =>
        val fieldsWithModels = properties.map { prop =>
          val isRequired = required.exists(_.apply(prop.name))

          val (tpe, modelOpt) = refSchemaToType(prop.name, prop.schema)

          (CaseClassField(isRequired, prop.name.transformInto[FieldName], tpe),
           modelOpt)
        }

        val fields = fieldsWithModels.map(_._1)
        val models = fieldsWithModels.toList.flatMap(_._2)

        val companion =
          if (models.isEmpty) None else Some(CompanionObject(models))

        CaseClass(typeName, fields, ExtendsClause.empty, companion)

      case NumberSchema(None) =>
        ValueClass(typeName, Primitive.Double)

      case StringSchema(None) =>
        ValueClass(typeName, Primitive.String)

      case comp: CompositeSchema =>
        convertCompositeSchema(typeName, comp)
    }
  }

  private def convertCompositeSchema(
    compositeName: TypeName,
    compositeSchema: CompositeSchema): ScalaModel = compositeSchema.kind match {
    case CompositeSchemaKind.OneOf | CompositeSchemaKind.AnyOf =>
      type S[A] = State[Int, A]

      val schemaz = compositeSchema.schemas.traverse[S, ScalaModel] {
        schemaOrRef =>
          //todo make this happen:
          //if it's a reference, we wrap in something of the same name
          //if it's a new type, we create it here directly and use a new name

          val getAndIncSyntheticNumber: S[Int] = State.get[Int] <* State.modify(_ + 1)

          val derivedWrappedName: S[SchemaName] = schemaOrRef match {
            case Left(ref) => SchemaName(refToTypeRef(ref.`$ref`).show).pure[S]
            case Right(StringSchema(None)) => SchemaName(Primitive.String.show).pure[S]
            case Right(NumberSchema(None)) => SchemaName(Primitive.Double.show).pure[S]
            case Right(_) => getAndIncSyntheticNumber.map(num => SchemaName(s"Anonymous$$$num"))
          }

          derivedWrappedName.map { derivedName =>
            convertSchemaOrRef(derivedName, schemaOrRef)
              .setExtendsClause(ExtendsClause(List(OrdinaryType(compositeName.value))))
          }
      }
      SealedTraitHierarchy(
        compositeName,
        schemaz.runA(1).value,
        compositeSchema.discriminator.map(convertDiscriminator)
      )
  }

  /**
    * Resolves a schema/reference to a type reference and (optionally) a synthetic type.
    * */
  private def refSchemaToType(
    name: SchemaName,
    schema: Reference.Able[Schema]): (TypeReference, Option[ScalaModel]) =
    schema match {
      case Left(ref)                 => (refToTypeRef(ref.`$ref`), None)
      case Right(NumberSchema(None)) => (Primitive.Double, None)
      case Right(StringSchema(None)) => (Primitive.String, None)
      case Right(StringSchema(Some(values))) =>
        val enumModel = Some(
          Enumerated(TypeName.parse(name.value),
                     Primitive.String,
                     values.map(ScalaLiteral.String(_))))

        (name.transformInto[OrdinaryType], enumModel)

      case Right(ArraySchema(items)) =>
        val (childType, childModel) = refSchemaToType(name, items)
        (ListType(childType), childModel)
    }
}
