package com.kubukoz.swaggins.openapi.model

import cats.effect.Sync
import com.kubukoz.swaggins.core.Parsing

class OpenApiParser[F[_]: Sync] {

  def parse(path: java.nio.file.Path): F[OpenAPI] =
    Parsing.yaml.parseFile[F, OpenAPI](path)
}
