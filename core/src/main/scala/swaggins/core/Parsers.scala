package swaggins.core

import java.nio.file.Path

import cats.MonadError
import cats.effect.{ContextShift, Sync}
import cats.implicits._
import fs2.text
import io.circe.{Decoder, Json}
import swaggins.core.Throwables.MonadThrow

import scala.concurrent.ExecutionContext

object Parsers {
  object yaml extends Parser(io.circe.yaml.parser.parse)
  object json extends Parser(io.circe.parser.parse)
}

class Parser private[core] (
  private val parse: String => Either[Throwable, Json]) {

  def parseFile[F[_]: FileReader: MonadThrow, T: Decoder](path: Path): F[T] = {
    FileReader[F].readFile(path).flatMap(decode[F, T])
  }

  private def decode[F[_]: MonadError[?[_], Throwable], T: Decoder](
    text: String): F[T] = {
    parse(text).liftTo[F].flatMap(_.as[T].liftTo[F])
  }
}

trait FileReader[F[_]] {
  def readFile(path: Path): F[String]
}

object FileReader {
  def apply[F[_]](implicit F: FileReader[F]): FileReader[F] = F

  def make[F[_]: Sync: ContextShift](
    blockingEc: ExecutionContext): FileReader[F] = new FileReader[F] {
    override def readFile(path: Path): F[String] = {
      fs2.io.file
        .readAll[F](path, blockingEc, 8192)
        .through(text.utf8Decode)
        .intersperse("\n")
        .compile
        .foldMonoid
    }
  }
}
