package swaggins.core
import java.util.concurrent.Executors

import cats.effect.{Resource, Sync}

import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService}

object ExecutionContexts {

  def unboundedCached[F[_]: Sync]
    : Resource[F, ExecutionContextExecutorService] =
    Resource.make(
      Sync[F].delay(
        ExecutionContext.fromExecutorService(Executors.newCachedThreadPool())))(
      ec => Sync[F].delay(ec.shutdown()))
}
