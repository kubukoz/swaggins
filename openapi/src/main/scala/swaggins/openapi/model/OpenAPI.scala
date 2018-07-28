/**
  * OpenAPI specification models and decoders.
  * */
package swaggins.openapi.model

import enumeratum._
import cats.implicits._
import cats.kernel.Order
import io.circe.generic.JsonCodec
import cats.data.{NonEmptyMap, NonEmptySet}
import io.circe.generic.extras.semiauto._
import io.circe.{Decoder, KeyDecoder}
import swaggins.core.implicits._

@JsonCodec(decodeOnly = true)
case class OpenAPI(openapi: String, info: Info, paths: Paths)

@JsonCodec(decodeOnly = true)
case class Info(version: String, title: String)

case class Paths(paths: NonEmptySet[Path])

object Paths {
  implicit val decoder: Decoder[Paths] =
    Decoder[NonEmptyMap[String, PathItem]].map { nem =>
      nem.keys.map { path =>
        Path(path, nem.lookup(path).get)
      }
    }.map(Paths(_))
}

@JsonCodec(decodeOnly = true)
case class Path(path: String, item: PathItem)

object Path {
  implicit val order: Order[Path] = Order.by(o => (o.path, o.item))
}

case class PathItem(value: NonEmptyMap[HttpMethod, Operation]) extends AnyVal {
  def get: Option[Operation] = value.lookup(HttpMethod.Get)
}

object PathItem {
  implicit val decoder: Decoder[PathItem] = deriveUnwrappedDecoder[PathItem]
  implicit val order: Order[PathItem]     = Order.by(_.value)
}

//Synthetic type - doesn't have a name in the OpenAPI spec.
sealed trait HttpMethod extends EnumEntry with Product with Serializable

object HttpMethod extends Enum[HttpMethod] {
  override def values: collection.immutable.IndexedSeq[HttpMethod] = findValues

  case object Get  extends HttpMethod
  case object Post extends HttpMethod

  implicit val decoder: KeyDecoder[HttpMethod] =
    KeyDecoder.instance(HttpMethod.withNameInsensitiveOption)

  implicit val order: Order[HttpMethod] = Order.by(indexOf)
}

@JsonCodec(decodeOnly = true)
case class Operation()
