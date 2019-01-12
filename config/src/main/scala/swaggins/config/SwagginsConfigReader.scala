package swaggins.config
import java.nio.file.Path

import cats.data.{NonEmptySet, Validated}
import cats.effect.{ContextShift, Sync}
import cats.implicits._
import swaggins.config.SwagginsConfigValidator.ValidatedNes
import swaggins.config.error.UnknownSourcesException
import swaggins.config.model.SwagginsConfig
import swaggins.config.model.shared.SourceIdentifier
import swaggins.core.Parsers

import scala.concurrent.ExecutionContext

//todo rethink interface
trait SwagginsConfigReader[F[_]] {
  def get(parsed: SwagginsConfig): F[SwagginsConfig]
  def read(path: Path): F[SwagginsConfig]
}

object SwagginsConfigReader {

  def make[F[_]: Sync: ContextShift](
    blockingEc: ExecutionContext): SwagginsConfigReader[F] =
    new SwagginsConfigReader[F] {

      def get(parsed: SwagginsConfig): F[SwagginsConfig] = {
        validate(parsed).toEither
          .leftMap[Throwable](UnknownSourcesException)
          .liftTo[F]
          .as(parsed)
      }

      private def validate(
        config: SwagginsConfig): ValidatedNes[SourceIdentifier, Unit] = {

        SwagginsConfigValidator.validateConfig(config)
      }

      def read(path: Path): F[SwagginsConfig] =
        Parsers.json.parseFile[F, SwagginsConfig](blockingEc, path)
    }
}

object SwagginsConfigValidator {
  type ValidatedNes[E, +R] = Validated[NonEmptySet[E], R]

  def validateConfig(
    config: SwagginsConfig): ValidatedNes[SourceIdentifier, Unit] = {
    val validateSources: ValidatedNes[SourceIdentifier, Unit] = {
      val usedSources = config.code.value.keys
      val isDeclared: SourceIdentifier => Boolean =
        config.sources.value.keys.contains

      usedSources.toList.traverse_ {
        _.valid
          .ensureOr(identity)(isDeclared)
          .toValidatedNel
          .leftMap[NonEmptySet[SourceIdentifier]](_.toNes)
      }
    }

    //not inlined on purpose
    validateSources
  }
}
