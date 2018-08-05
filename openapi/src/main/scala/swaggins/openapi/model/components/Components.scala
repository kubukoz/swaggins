package swaggins.openapi.model.components

import cats.Show
import cats.data.NonEmptyMap
import cats.implicits._
import cats.kernel.Order
import io.circe.generic.JsonCodec
import io.circe.generic.extras.semiauto._
import io.circe.{Decoder, KeyDecoder}
import swaggins.core.implicits._
import swaggins.openapi.model.shared.{Reference, Schema}

@JsonCodec(decodeOnly = true)
case class Components(schemas: NonEmptyMap[SchemaName, Reference.Able[Schema]])

/**
  * $synthetic
  * */
case class SchemaName(value: String) extends AnyVal

object SchemaName {
  implicit val order: Order[SchemaName]     = Order.by(_.value)
  implicit val decoder: Decoder[SchemaName] = deriveUnwrappedDecoder
  implicit val keyDecoder: KeyDecoder[SchemaName] =
    KeyDecoder.instance(apply(_).some)
}
