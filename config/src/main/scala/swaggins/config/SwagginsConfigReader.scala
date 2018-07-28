package swaggins.config
import java.nio.file.Path

import cats.data.ValidatedNel
import cats.effect.Sync
import cats.implicits._
import swaggins.config.model.SwagginsConfig
import swaggins.config.model.shared.SourceIdentifier
import swaggins.core.Parsers

class SwagginsConfigReader[F[_]: Sync] {

  def get(input: Path): F[SwagginsConfig] = {
    read(input).flatTap {
      validate(_).toEither
        .leftMap(_.toList.mkString(", "))
        .leftMap[Throwable](new RuntimeException(_))
        .liftTo[F]
    }
  }

  private def validate(config: SwagginsConfig): ValidatedNel[String, Unit] = {
    val validateSources: ValidatedNel[String, Unit] = {
      val usedSources                             = config.code.value.keySet
      val isDeclared: SourceIdentifier => Boolean = config.sources.value.keySet

      usedSources.toList.traverse_[ValidatedNel[String, ?], SourceIdentifier] {
        _.valid.ensureOr(unknownSource)(isDeclared).toValidatedNel
      }
    }

    validateSources
  }

  private def unknownSource(source: SourceIdentifier): String = {
    s"""Unknown source: $source. Did you define it in the "sources" property?"""
  }

  def read(path: Path): F[SwagginsConfig] =
    Parsers.json.parseFile[F, SwagginsConfig](path)
}
