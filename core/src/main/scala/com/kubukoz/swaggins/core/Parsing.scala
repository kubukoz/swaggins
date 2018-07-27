package com.kubukoz.swaggins.core

import java.nio.file.Path

import cats.effect.Sync
import cats.implicits._
import com.kubukoz.swaggins.core.Parser.readFile
import fs2.text
import io.circe.{Decoder, Json}

object Parsing {
  object yaml extends Parser(io.circe.yaml.parser.parse)
  object json extends Parser(io.circe.parser.parse)
}

class Parser(parse: Parser.Fun) {

  def parseFile[F[_]: Sync, T: Decoder](path: Path): F[T] = {
    readFile(path).flatMap(decode(_, parse))
  }

  private def decode[F[_]: Sync, T: Decoder](text: String,
                                             parser: Parser.Fun): F[T] = {

    parser(text).liftTo[F].flatMap(_.as[T].leftWiden[Throwable].liftTo[F])
  }
}

object Parser {
  type Fun = String => Either[Throwable, Json]

  private def readFile[T: Decoder, F[_]: Sync](path: Path): F[String] = {
    fs2.io.file
      .readAll[F](path, 8192)
      .through(text.utf8Decode)
      .intersperse("\n")
      .compile
      .foldMonoid
  }
}
