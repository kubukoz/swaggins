package swaggins.config
import java.nio.file.Path

import cats.effect.Sync
import cats.implicits._
import io.circe.generic.extras.semiauto._
import io.circe.{Decoder, KeyDecoder}
import swaggins.core.Parsing

class SwagginsConfigParser[F[_]: Sync] {

  def parse(path: Path): F[SwagginsConfig] =
    Parsing.json.parseFile[F, SwagginsConfig](path)
}

case class SwagginsConfig(sources: Map[SpecRepository, Unit]) extends AnyVal

object SwagginsConfig {
  implicit val decoder: Decoder[SwagginsConfig] =
    deriveUnwrappedDecoder[SwagginsConfig]
}

case class SpecRepository(sourceKey: Option[String], user: String)

object SpecRepository {
  implicit val decoder: KeyDecoder[SpecRepository] = {
    KeyDecoder.instance {
      _.split(":").toList match {
        case key :: user :: Nil => SpecRepository(Some(key), user).some
        case user :: Nil        => SpecRepository(None, user).some
        case _                  => none
      }
    }
  }
}
