package com.kubukoz.swaggins.cli.spec
import com.kubukoz.swaggins.cli.spec.Path.Methods
import io.circe.{Decoder, KeyDecoder}
import enumeratum._
import io.circe.generic.JsonCodec

@JsonCodec(decodeOnly = true)
case class Spec(paths: Paths)

@JsonCodec(decodeOnly = true)
case class PathBody()

@JsonCodec(decodeOnly = true)
case class Path(path: String, methods: Methods)

object Path {
  type Methods = Map[HttpMethod, PathBody]
}

case class Paths(paths: List[Path]) extends AnyVal

object Paths {
  implicit val decoder: Decoder[Paths] = Decoder[Map[String, Methods]].map {
    _.toList.map { case (path, methods) =>
      Path(path, methods)
    }
  }.map(Paths(_))
}

sealed trait HttpMethod extends EnumEntry with Product with Serializable

object HttpMethod extends Enum[HttpMethod] {
  override def values: collection.immutable.IndexedSeq[HttpMethod] = findValues

  case object Get  extends HttpMethod
  case object Post extends HttpMethod

  implicit val decoder: KeyDecoder[HttpMethod] = KeyDecoder.instance(HttpMethod.withNameInsensitiveOption)
}
