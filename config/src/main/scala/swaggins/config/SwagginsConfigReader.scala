package swaggins.config
import java.nio.file.Path

import cats.data.ValidatedNel
import cats.effect.Sync
import cats.implicits._
import swaggins.config.error.UnknownSourcesException
import swaggins.config.model.SwagginsConfig
import swaggins.config.model.shared.SourceIdentifier
import swaggins.core.Parsers

class SwagginsConfigReader[F[_]: Sync] {

  def get(parsed: SwagginsConfig): F[SwagginsConfig] = {
    validate(parsed).toEither
      .leftMap[Throwable](UnknownSourcesException)
      .liftTo[F]
      .as(parsed)
  }

  private def validate(
    config: SwagginsConfig): ValidatedNel[SourceIdentifier, Unit] = {
    val validateSources: ValidatedNel[SourceIdentifier, Unit] = {
      val usedSources = config.code.value.keys
      val isDeclared: SourceIdentifier => Boolean =
        config.sources.value.keys.contains

      usedSources.toList
        .traverse_[ValidatedNel[SourceIdentifier, ?], SourceIdentifier] {
          _.valid.ensureOr(identity)(isDeclared).toValidatedNel
        }
    }

    validateSources
  }

  def read(path: Path): F[SwagginsConfig] =
    Parsers.json.parseFile[F, SwagginsConfig](path)
}
