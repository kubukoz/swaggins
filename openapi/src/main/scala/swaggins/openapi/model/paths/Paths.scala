package swaggins.openapi.model.paths

import cats.data.{NonEmptyMap, NonEmptySet}
import cats.implicits._
import cats.kernel.Order
import enumeratum._
import io.circe.generic.JsonCodec
import io.circe.generic.extras.semiauto._
import io.circe.{Decoder, KeyDecoder}
import swaggins.core.implicits._
import swaggins.openapi.model.shared.{Reference, Schema}

import scala.util.Try

case class Paths(paths: NonEmptySet[Path])

object Paths {
  implicit val decoder: Decoder[Paths] =
    Decoder[NonEmptyMap[String, PathItem]].map { nem =>
      nem.keys.map { path =>
        Path(path, nem.lookup(path).get)
      }
    }.map(Paths(_))
}

/**
  * $synthetic
  * */
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

/**
  * $synthetic
  * */
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
case class Operation(responses: Responses)

case class Responses(value: NonEmptyMap[StatusCode, Response]) extends AnyVal

object Responses {
  implicit val decoder: Decoder[Responses] = deriveUnwrappedDecoder
}

/**
  * $synthetic
  * */
case class StatusCode(value: Int) extends AnyVal

object StatusCode {
  implicit val order: Order[StatusCode] = Order.by(_.value)
  implicit val decoder: KeyDecoder[StatusCode] = KeyDecoder.instance { str =>
    Try(str.toInt).toOption.map(apply)
  }
}

@JsonCodec(decodeOnly = true)
case class Response(content: Option[NonEmptyMap[ContentType, MediaType]])

/**
  * $synthetic
  * */
case class ContentType(value: String) extends AnyVal

object ContentType {
  implicit val order: Order[ContentType] = Order.by(_.value)

  implicit val decoder: KeyDecoder[ContentType] =
    KeyDecoder.instance(apply(_).some)
}

@JsonCodec(decodeOnly = true)
case class MediaType(schema: Option[Reference.Able[Schema]])
