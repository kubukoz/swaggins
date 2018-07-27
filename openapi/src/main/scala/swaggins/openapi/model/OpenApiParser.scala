package swaggins.openapi.model

import cats.effect.Sync
import swaggins.core.Parsing

class OpenApiParser[F[_]: Sync] {

  def parse(path: java.nio.file.Path): F[OpenAPI] =
    Parsing.yaml.parseFile[F, OpenAPI](path)
}
