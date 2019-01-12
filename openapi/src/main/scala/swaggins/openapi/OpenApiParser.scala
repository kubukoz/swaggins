package swaggins.openapi

import swaggins.core.Throwables.MonadThrow
import swaggins.core.{FileReader, Parsers}
import swaggins.openapi.model.OpenAPI

trait OpenApiParser[F[_]] {
  def parse(path: java.nio.file.Path): F[OpenAPI]
}

object OpenApiParser {

  def make[F[_]: FileReader: MonadThrow]: OpenApiParser[F] =
    path => Parsers.yaml.parseFile[F, OpenAPI](path)
}
