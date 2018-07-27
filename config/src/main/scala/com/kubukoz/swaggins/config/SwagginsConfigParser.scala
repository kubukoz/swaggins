package com.kubukoz.swaggins.config
import java.nio.file.Path

import cats.effect.Sync
import com.kubukoz.swaggins.core.Parsing
import io.circe.{Decoder, KeyDecoder}
import cats.implicits._
import io.circe.generic.extras.semiauto._

class SwagginsConfigParser[F[_]: Sync] {

  def parse(path: Path): F[SwagginsConfig] =
    Parsing.json.parseFile[F, SwagginsConfig](path)
}

case class SwagginsConfig(sources: Map[SpecSource, Unit]) extends AnyVal

object SwagginsConfig {
  implicit val decoder: Decoder[SwagginsConfig] =
    deriveUnwrappedDecoder[SwagginsConfig]
}

case class SpecSource(key: Option[String], user: String)

object SpecSource {
  implicit val decoder: KeyDecoder[SpecSource] = {
    KeyDecoder.instance {
      _.split(":").toList match {
        case key :: user :: Nil => SpecSource(Some(key), user).some
        case user :: Nil        => SpecSource(None, user).some
        case _                  => none
      }
    }
  }
}
