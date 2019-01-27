package swaggins.openapi.model.shared

import cats.data.NonEmptyList
import io.circe.Decoder
import swaggins.openapi.model.components.SchemaName
import cats.implicits._
import io.circe.generic.semiauto._

sealed trait RefOrSchema extends Product with Serializable

object RefOrSchema {
  final case class Reference(`$ref`: ReferenceRef) extends RefOrSchema
  final case class InlineSchema(schema: Schema)    extends RefOrSchema

  implicit val decoder: Decoder[RefOrSchema] =
    NonEmptyList
      .of(
        deriveDecoder[Reference],
        Decoder[Schema].map(InlineSchema)
      )
      .map(_.widen[RefOrSchema])
      .reduceK

}

/**
  * $synthetic
  * */
sealed trait ReferenceRef extends Product with Serializable

object ReferenceRef {
  final case class ComponentRef(name: SchemaName) extends ReferenceRef

  object ComponentRef {
    implicit val decoder: Decoder[ComponentRef] = {
      val pattern = """\#\/components\/schemas\/(.+)""".r

      Decoder[String].emap {
        case pattern(name) => ComponentRef(SchemaName(name)).asRight
        case str           => s"Invalid component reference: $str".asLeft
      }
    }
  }

  implicit val decoder: Decoder[ReferenceRef] =
    NonEmptyList
      .of(
        Decoder[ComponentRef].widen[ReferenceRef]
      )
      .reduceK

}
