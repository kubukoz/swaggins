package swaggins.generator.convert

import cats.data._
import cats.implicits._
import cats.{Applicative, Monad}
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
    schemaOrRef: RefOrSchema
  ): F[ModelWithCompanion]
}

object Converters {
  def apply[F[_]](implicit F: Converters[F]): Converters[F] = F

  def make[F[_]: Packages.Local: Monad]: Converters[F] =
    new Converters[F] {
      implicit val converters: Converters[F] = this
      override def convertSchemaOrRef(
        schemaName: SchemaName,
        schemaOrRef: RefOrSchema): F[ModelWithCompanion] =
        Converters.generateSchemasForRef[F](TypeName.parse(schemaName.value),
                                            schemaOrRef)
    }

  def generateSchemasForRef[F[_]: Packages.Local: Monad](
    typeName: TypeName,
    refOrSchema: RefOrSchema
  ): F[ModelWithCompanion] =
    refOrSchema match {
      case RefOrSchema.InlineSchema(schema) =>
        convertSchema(typeName, schema)
      case RefOrSchema.Reference(ref) =>
        refToTypeRef[F](ref).map { typeRef =>
          val klazz = ScalaModel.finalCaseClass(
            typeName,
            NonEmptyList.one(
              ClassField(
                FieldName("value"),
                typeRef
              )
            ),
            ExtendsClause.empty
          )

          ModelWithCompanion.justClass(klazz)
        }
    }

  private def refToTypeName(ref: ReferenceRef): TypeName = ref match {
    case ComponentRef(name) => TypeName.parse(name.value)
  }

  private def refToTypeRef[F[_]: Packages.Ask](
    ref: ReferenceRef): F[TypeReference] =
    TypeReference.byName(refToTypeName(ref))

  /**
    * Converts an OpenAPI Schema to a Scala model (e.g. a case class or an ADT).
    * */
  def convertSchema[F[_]: Packages.Local: Monad](
    typeName: TypeName,
    schema: Schema): F[ModelWithCompanion] = {
    schema match {
      case obj: ObjectSchema =>
        convertObjectSchema(typeName, obj)

      case NumberSchema(None) =>
        ModelWithCompanion
          .justClass(
            ScalaModel.valueClass(typeName, TypeReference.double))
          .pure[F]

      case StringSchema(None) =>
        ModelWithCompanion
          .justClass(
            ScalaModel.valueClass(typeName, TypeReference.string))
          .pure[F]

      case comp: CompositeSchema =>
        convertCompositeSchema[F](typeName, comp)
    }
  }

  def convertCompositeSchema[F[_]: Packages.Local: Monad](
    compositeName: TypeName,
    compositeSchema: CompositeSchema
  ): F[ModelWithCompanion] = {
    val extendCompositeSchemaRoot: ModelWithCompanion => F[ModelWithCompanion] =
      model =>
        TypeReference.byName[F](compositeName).map { tpe =>
          ModelWithCompanion.klassExtendsClause.set(
            ExtendsClause(List(tpe))
          )(model)
      }

    compositeSchema.kind match {
      case CompositeSchemaKind.OneOf | CompositeSchemaKind.AnyOf =>
        type S[A] = StateT[F, Int, A]

        val getAndIncSyntheticNumber: S[Int] = StateT.get[F, Int] <* StateT
          .modify(_ + 1)

        val schemaz: F[NonEmptyList[ModelWithCompanion]] =
          compositeSchema.schemas.nonEmptyTraverse { schemaOrRef =>
            val derivedWrappedName: S[TypeName] = schemaOrRef match {
              case RefOrSchema.Reference(ref) =>
                refToTypeName(ref).pure[S]
              case RefOrSchema.InlineSchema(StringSchema(None)) =>
                PrimitiveNames.string.pure[S]
              case RefOrSchema.InlineSchema(NumberSchema(None)) =>
                PrimitiveNames.double.pure[S]
              case RefOrSchema.InlineSchema(_) =>
                getAndIncSyntheticNumber.map(
                  num => TypeName.raw(s"Anonymous$$$num")
                )
            }

            derivedWrappedName.flatMapF { name =>
              Packages
                .Local[F]
                .local(_.append(PackageName(compositeName.value))) {
                  generateSchemasForRef[F](name, schemaOrRef)
                }
            }
          }.runA(1)

        schemaz.flatMap(_.nonEmptyTraverse(extendCompositeSchemaRoot)).map {
          models =>
            ScalaModel.sealedTraitHierarchy(
              compositeName,
              models.flatMap(_.asNel)
            )
        }
    }
  }

  /**
    * Generates models for an object schema.
    * */
  def convertObjectSchema[F[_]: Packages.Local: Applicative](
    typeName: TypeName,
    schema: ObjectSchema): F[ModelWithCompanion] =
    Packages.Local.local(_.append(PackageName(typeName.value))) {

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
  def propertyToFieldWithModel[F[_]: Packages.Ask: Applicative](
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
  def refSchemaToType[F[_]: Packages.Ask: Applicative](
    name: SchemaName,
    schema: RefOrSchema
  ): F[(TypeReference, Option[ModelWithCompanion])] = {
    schema match {
      case RefOrSchema.Reference(ref) => refToTypeRef(ref).tupleRight(None)
      case RefOrSchema.InlineSchema(NumberSchema(None)) =>
        (TypeReference.double, None).pure[F].widen
      case RefOrSchema.InlineSchema(StringSchema(None)) =>
        (TypeReference.string, None).pure[F].widen
      case RefOrSchema.InlineSchema(StringSchema(Some(values))) =>
        val enumModel =
          ScalaModel.enumeration(
            TypeName.parse(name.value),
            TypeReference.string,
            values.map(ScalaLiteral.string(_))
          )

        (
          TypeReference.byName(TypeName.parse(name.value)),
          enumModel.map(_.some)
        ).tupled

      case RefOrSchema.InlineSchema(ArraySchema(items)) =>
        refSchemaToType(name, items).map {
          case (childType, childModel) =>
            (TypeReference.listOf(childType), childModel)
        }
    }
  }
}
