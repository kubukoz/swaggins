package swaggins.openapi

import cats.effect.Sync
import swaggins.core.Parsers
import swaggins.openapi.model.OpenAPI

class OpenApiParser[F[_]: Sync] {

  def parse(path: java.nio.file.Path): F[OpenAPI] =
    Parsers.yaml.parseFile[F, OpenAPI](path)
}
