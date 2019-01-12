package swaggins.core

import java.nio.file.Path

import cats.effect.{ContextShift, Sync}
import cats.implicits._
import fs2.text
import io.circe.{Decoder, Json}
import swaggins.core.Parser.readFile

import scala.concurrent.ExecutionContext

object Parsers {
  object yaml extends Parser(io.circe.yaml.parser.parse)
  object json extends Parser(io.circe.parser.parse)
}

class Parser private[core] (
  private val parse: String => Either[Throwable, Json]) {

  def parseFile[F[_]: Sync: ContextShift, T: Decoder](
    blockingEc: ExecutionContext,
    path: Path): F[T] = {
    readFile(blockingEc, path).flatMap(decode[F, T])
  }

  private def decode[F[_]: Sync, T: Decoder](text: String): F[T] = {
    parse(text).liftTo[F].flatMap(_.as[T].liftTo[F])
  }
}

object Parser {

  def readFile[F[_]: Sync: ContextShift](blockingEc: ExecutionContext,
                                         path: Path): F[String] = {
    fs2.io.file
      .readAll[F](path, blockingEc, 8192)
      .through(text.utf8Decode)
      .intersperse("\n")
      .compile
      .foldMonoid
  }
}
