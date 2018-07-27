/**
  * OpenAPI specification models and decoders.
  * */
package com.kubukoz.swaggins.openapi.parse

import enumeratum._
import io.circe.generic.JsonCodec
import io.circe.generic.extras.semiauto._
import io.circe.{Decoder, KeyDecoder}

@JsonCodec(decodeOnly = true)
case class OpenAPI(paths: Paths)

case class Paths(paths: List[Path])

object Paths {
  implicit val decoder: Decoder[Paths] = Decoder[Map[String, PathItem]].map {
    _.toList.map {
      case (path, methods) =>
        Path(path, methods)
    }
  }.map(Paths(_))
}

@JsonCodec(decodeOnly = true)
case class Path(path: String, item: PathItem)

case class PathItem(value: Map[HttpMethod, Operation]) extends AnyVal {
  def get: Option[Operation] = value.get(HttpMethod.Get)
}

object PathItem {
  implicit val decoder: Decoder[PathItem] = deriveUnwrappedDecoder[PathItem]
}

//Synthetic type - doesn't have a name in the OpenAPI spec.
sealed trait HttpMethod extends EnumEntry with Product with Serializable

object HttpMethod extends Enum[HttpMethod] {
  override def values: collection.immutable.IndexedSeq[HttpMethod] = findValues

  case object Get  extends HttpMethod
  case object Post extends HttpMethod

  implicit val decoder: KeyDecoder[HttpMethod] = KeyDecoder.instance(HttpMethod.withNameInsensitiveOption)
}

@JsonCodec(decodeOnly = true)
case class Operation()
