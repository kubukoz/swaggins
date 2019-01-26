package swaggins.config.error
import cats.ApplicativeError
import cats.data.NonEmptyList
import swaggins.config.model.shared.SourceIdentifier

trait ConfigValidationError extends Product with Serializable

object ConfigValidationError {

  type EitherNelT[F[_], A] =
    cats.data.EitherT[F, NonEmptyList[ConfigValidationError], A]

  type EitherNel[A] =
    cats.data.EitherNel[ConfigValidationError, A]

  type ErrorsNel[F[_]] =
    ApplicativeError[F, NonEmptyList[ConfigValidationError]]

  object RaiseNel {
    def apply[F[_]](implicit F: ErrorsNel[F]): ErrorsNel[F] = F

    def raiseOne[F[_]: ErrorsNel, A](error: ConfigValidationError): F[A] =
      RaiseNel[F].raiseError(NonEmptyList.one(error))
  }

  final case class UnknownSources(source: SourceIdentifier)
      extends ConfigValidationError
}
