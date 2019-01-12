package swaggins.openapi

import cats.effect.{ContextShift, Sync}
import swaggins.core.Parsers
import swaggins.openapi.model.OpenAPI

import scala.concurrent.ExecutionContext

trait OpenApiParser[F[_]] {
  def parse(path: java.nio.file.Path): F[OpenAPI]
}

object OpenApiParser {

  def make[F[_]: Sync: ContextShift](
    blockingEc: ExecutionContext): OpenApiParser[F] =
    path => Parsers.yaml.parseFile[F, OpenAPI](blockingEc, path)
}
