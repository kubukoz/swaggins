package swaggins.config
import java.nio.file.Path

import cats.{Applicative, Parallel}
import cats.implicits._
import cats.temp.par._
import swaggins.config.error.ConfigValidationError
import swaggins.config.error.ConfigValidationError.UnknownSources
import swaggins.config.model.SwagginsConfig
import swaggins.config.model.shared.SourceIdentifier
import swaggins.core.Throwables.MonadThrow
import swaggins.core.{FileReader, Parsers}

trait SwagginsConfigReader[F[_]] {
  def read(path: Path): F[SwagginsConfig]
}

object SwagginsConfigReader {

  def make[F[_]: MonadThrow: FileReader: SwagginsConfigValidator]
    : SwagginsConfigReader[F] =
    Parsers.json
      .parseFile[F, SwagginsConfig](_)
      .flatTap(SwagginsConfigValidator[F].validateConfig)
}

trait SwagginsConfigValidator[F[_]] {
  def validateConfig(config: SwagginsConfig): F[Unit]
}

object SwagginsConfigValidator {

  def apply[F[_]](
    implicit F: SwagginsConfigValidator[F]): SwagginsConfigValidator[F] = F

  def make[F[_]: NonEmptyPar: Applicative: ConfigValidationError.ErrorsNel]
    : SwagginsConfigValidator[F] = config => {
    val usedSources = config.code.value.keys
    val isDeclared: SourceIdentifier => Boolean =
      config.sources.value.keys(_)

    Parallel.parNonEmptyTraverse_(usedSources.toNonEmptyList) {
      case src if isDeclared(src) => Applicative[F].unit
      case src =>
        ConfigValidationError.RaiseNel.raiseOne[F, Unit](UnknownSources(src))
    }
  }
}
