package swaggins.core

import java.nio.file.Path

import cats.effect.Sync
import cats.implicits._
import fs2.text
import io.circe.{Decoder, Json}
import swaggins.core.Parser.readFile

object Parsers {
  object yaml extends Parser(io.circe.yaml.parser.parse)
  object json extends Parser(io.circe.parser.parse)
}

class Parser(parse: String => Either[Throwable, Json]) {

  def parseFile[F[_]: Sync, T: Decoder](path: Path): F[T] = {
    readFile(path).flatMap(decode(_))
  }

  private def decode[F[_]: Sync, T: Decoder](text: String): F[T] = {
    parse(text).liftTo[F].flatMap(_.as[T].leftWiden[Throwable].liftTo[F])
  }
}

object Parser {
  private def readFile[T: Decoder, F[_]: Sync](path: Path): F[String] = {
    fs2.io.file
      .readAll[F](path, 8192)
      .through(text.utf8Decode)
      .intersperse("\n")
      .compile
      .foldMonoid
  }
}
