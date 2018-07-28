package swaggins.openapi.model

import cats.effect.Sync
import swaggins.core.Parsers

class OpenApiParser[F[_]: Sync] {

  def parse(path: java.nio.file.Path): F[OpenAPI] =
    Parsers.yaml.parseFile[F, OpenAPI](path)
}
