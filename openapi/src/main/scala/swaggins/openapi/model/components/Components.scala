package swaggins.openapi.model.components

import cats.data.NonEmptyMap
import cats.implicits._
import cats.kernel.Order
import scalaz.deriving
import io.circe.generic.extras.semiauto._
import io.circe.{Decoder, KeyDecoder}
import swaggins.openapi.model.shared.RefOrSchema

@deriving(Decoder)
final case class Components(schemas: NonEmptyMap[SchemaName, RefOrSchema])

/**
  * $synthetic
  * */
final case class SchemaName(value: String) extends AnyVal

object SchemaName {
  implicit val order: Order[SchemaName]     = Order.by(_.value)
  implicit val decoder: Decoder[SchemaName] = deriveUnwrappedDecoder
  implicit val keyDecoder: KeyDecoder[SchemaName] =
    KeyDecoder.instance(apply(_).some)
}
