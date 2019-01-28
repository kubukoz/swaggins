package swaggins.openapi.model.paths

import cats.data.{NonEmptyMap, NonEmptySet}
import cats.implicits._
import cats.kernel.Order
import enumeratum._
import io.circe.{Decoder, KeyDecoder}
import swaggins.core.implicits._
import swaggins.openapi.model.shared.RefOrSchema
import scalaz.{deriving, xderiving}

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
@deriving(Decoder, Order)
final case class Path(path: String, item: PathItem)

@xderiving(Decoder, Order)
final case class PathItem(value: NonEmptyMap[HttpMethod, Operation])
    extends AnyVal {
  def get: Option[Operation] = value.lookup(HttpMethod.Get)
}

object PathItem {
  //empty object for scalaz-deriving
}

/**
  * $synthetic
  * */
sealed trait HttpMethod extends EnumEntry with Product with Serializable

object HttpMethod extends Enum[HttpMethod] {
  override val values: collection.immutable.IndexedSeq[HttpMethod] = findValues

  case object Get  extends HttpMethod
  case object Post extends HttpMethod

  implicit val decoder: KeyDecoder[HttpMethod] =
    KeyDecoder.instance(HttpMethod.withNameInsensitiveOption)

  implicit val order: Order[HttpMethod] = Order.by(indexOf)
}

@deriving(Decoder)
final case class Operation(responses: Responses)

@xderiving(Decoder)
final case class Responses(value: NonEmptyMap[StatusCode, Response])
    extends AnyVal

object Responses {
//   //empty companion object to fix xderiving
}

/**
  * $synthetic
  * */
@xderiving(Order, KeyDecoder)
final case class StatusCode(value: Int) extends AnyVal

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
