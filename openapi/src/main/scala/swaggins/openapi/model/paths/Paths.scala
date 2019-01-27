package swaggins.openapi.model.paths

import cats.data.{NonEmptyMap, NonEmptySet}
import cats.implicits._
import cats.kernel.Order
import enumeratum._
import io.circe.generic.extras.semiauto._
import io.circe.{Decoder, KeyDecoder}
import swaggins.core.implicits._
import swaggins.openapi.model.shared.RefOrSchema
import scalaz.{deriving, xderiving}

import scala.util.Try

final case class Paths(paths: NonEmptySet[Path])

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
@deriving(Decoder)
final case class Path(path: String, item: PathItem)

object Path {
  implicit val order: Order[Path] = Order.by(o => (o.path, o.item))
}

final case class PathItem(value: NonEmptyMap[HttpMethod, Operation]) extends AnyVal {
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

@deriving(Decoder)
final case class Operation(responses: Responses)

@xderiving(Decoder)
final case class Responses(value: NonEmptyMap[StatusCode, Response]) extends AnyVal

object Responses {
//   //empty companion object to fix xderiving 
}

/**
  * $synthetic
  * */
final case class StatusCode(value: Int) extends AnyVal

object StatusCode {
  implicit val order: Order[StatusCode] = Order.by(_.value)
  implicit val decoder: KeyDecoder[StatusCode] = KeyDecoder.instance { str =>
    Try(str.toInt).toOption.map(apply)
  }
}

@deriving(Decoder)
final case class Response(content: Option[Content])

/**
  * $synthetic
  * */
final case class Content(json: MediaType) extends AnyVal

object Content {
  implicit val decoder: Decoder[Content] =
    Decoder[MediaType].prepare(_.downField("application/json")).map(apply)
}

@deriving(Decoder)
final case class MediaType(schema: Option[RefOrSchema])
