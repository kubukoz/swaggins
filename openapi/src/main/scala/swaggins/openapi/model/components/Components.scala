package swaggins.openapi.model.components

import cats.data.NonEmptyMap
import cats.implicits._
import cats.kernel.Order
import scalaz.{deriving, xderiving}
import io.circe.{Decoder, KeyDecoder}
import swaggins.openapi.model.shared.RefOrSchema
import swaggins.core.implicits._

@deriving(Decoder)
final case class Components(schemas: NonEmptyMap[SchemaName, RefOrSchema])

/**
  * $synthetic
  * */
@xderiving(Order, Decoder, KeyDecoder)
final case class SchemaName(value: String) extends AnyVal
