package swaggins.generator
import cats.effect.Sync
import cats.implicits._
import fs2.Stream
import swaggins.core.implicits._
import swaggins.openapi.model.OpenAPI
import swaggins.openapi.model.components.SchemaName
import swaggins.openapi.model.shared.Reference.Able
import swaggins.openapi.model.shared._
import swaggins.scala.ast
import swaggins.scala.ast._
import io.scalaland.chimney.dsl._
import swaggins.openapi.model.shared.ReferenceRef.ComponentRef
import swaggins.scala.ast.TypeReference.{ListType, OrdinaryType}

trait Generator[F[_]] {
  def generate(spec: OpenAPI): Stream[F, GeneratedFile]
}

class ScalaCaseClassGenerator[F[_]: Sync] extends Generator[F] {

  private val refToType: ReferenceRef => OrdinaryType = {
    case ComponentRef(name) => name.transformInto[OrdinaryType]
  }

  def generate(spec: OpenAPI): Stream[F, GeneratedFile] = {
    val componentList: List[(SchemaName, Able[Schema])] =
      spec.components.schemas.toSortedMap.toList

    val componentStrings: Stream[F, String] = Stream
      .emits(componentList)
      .map {
        case (name, Right(schema)) => convertSchema(name, schema)
      }
      .map(_.show)

    Stream.emit(componentStrings).evalMap { fileStream =>
      fileStream.compile.toList.map { lines =>
        GeneratedFile("models.scala", lines.mkString_("", "\n", "\n"))
      }
    }
  }

  /**
    * Converts an OpenAPI Schema to a Scala model (e.g. a case class or an ADT).
    * */
  private def convertSchema(schemaName: SchemaName,
                            schema: Schema): ScalaModel = {
    schema match {
      case ObjectSchema(required, properties) =>
        val fields = properties.map { prop =>
          val isRequired = required.exists(_.apply(prop.name))

          CaseClassField(isRequired,
                         prop.name.transformInto[FieldName],
                         refSchemaToType(prop.schema))
        }

        ast.CaseClass(schemaName.transformInto[TypeName], fields)

      case NumberSchema(None) =>
        ValueClass(schemaName.transformInto[TypeName],
                   TypeReference.Primitive.Double)
    }
  }

  private def refSchemaToType(schema: Reference.Able[Schema]): TypeReference =
    schema match {
      case Left(ref)                 => refToType(ref.`$ref`)
      case Right(NumberSchema(None)) => TypeReference.Primitive.Double
      case Right(StringSchema(None)) => TypeReference.Primitive.String
      case Right(ArraySchema(items)) => ListType(refSchemaToType(items))
      case Right(_: ObjectSchema) =>
        ??? //needs to be added as a new synthetic type - most likely change type of method to either and the caller to list
    }
}
