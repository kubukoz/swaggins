package swaggins.config
import java.nio.file.Path

import cats.data.{NonEmptySet, Validated}
import cats.effect.Sync
import cats.implicits._
import swaggins.config.SwagginsConfigValidator.ValidatedNes
import swaggins.config.error.UnknownSourcesException
import swaggins.config.model.SwagginsConfig
import swaggins.config.model.shared.SourceIdentifier
import swaggins.core.Parsers
import swaggins.core.implicits._

class SwagginsConfigReader[F[_]: Sync] {

  def get(parsed: SwagginsConfig): F[SwagginsConfig] = {
    validate(parsed).toEither
      .leftMap[Throwable](UnknownSourcesException)
      .liftTo[F]
      .as(parsed)
  }

  private def validate(
    config: SwagginsConfig): ValidatedNes[SourceIdentifier, Unit] = {

    SwagginsConfigValidator.validateSources(config)
  }

  def read(path: Path): F[SwagginsConfig] =
    Parsers.json.parseFile[F, SwagginsConfig](path)
}

object SwagginsConfigValidator {
  type ValidatedNes[E, +R] = Validated[NonEmptySet[E], R]

  def validateSources(
    config: SwagginsConfig): ValidatedNes[SourceIdentifier, Unit] = {
    val validateSources: ValidatedNes[SourceIdentifier, Unit] = {
      val usedSources = config.code.value.keys
      val isDeclared: SourceIdentifier => Boolean =
        config.sources.value.keys.contains

      usedSources.toList
        .traverse_[ValidatedNes[SourceIdentifier, ?], SourceIdentifier] {
          _.valid
            .ensureOr(identity)(isDeclared)
            .toValidatedNel
            .leftMap[NonEmptySet[SourceIdentifier]](_.toNes)
        }
    }
    validateSources
  }
}
