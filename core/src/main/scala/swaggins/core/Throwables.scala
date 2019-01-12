package swaggins.core
import cats.MonadError

object Throwables {
  type MonadThrow[F[_]] = MonadError[F, Throwable]
}
