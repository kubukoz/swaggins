package com.kubukoz.swaggins.openapi.model

import cats.effect.Sync
import cats.implicits._
import io.circe.yaml.parser
import fs2._

object OpenApiParser {

  def parse[F[_]: Sync](path: java.nio.file.Path): F[OpenAPI] = {
    io.file
      .readAll[F](path, 8192)
      .through(text.utf8Decode)
      .intersperse("\n")
      .compile
      .foldMonoid
      .flatMap(parser.parse(_).leftWiden[Throwable].liftTo[F])
      .flatMap(_.as[OpenAPI].leftWiden[Throwable].liftTo[F])
  }
}
