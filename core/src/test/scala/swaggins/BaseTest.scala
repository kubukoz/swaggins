package swaggins

import java.nio.file.{Path, Paths}

import cats.effect.{ContextShift, IO, Timer}
import cats.implicits._
import org.scalatest.{Assertion, Matchers, WordSpec}
import swaggins.core.{ExecutionContexts, FileReader}

import scala.concurrent.ExecutionContext

trait BaseTest extends WordSpec with Matchers {
  implicit val cs: ContextShift[IO] = IO.contextShift(ExecutionContext.global)
  implicit val timer: Timer[IO]     = IO.timer(ExecutionContext.global)

  def filePath(name: String): Path = {
    Paths.get(getClass.getResource(name).toURI)
  }

  def fileContent(name: String): IO[String] =
    ExecutionContexts.unboundedCached[IO].map(FileReader.make[IO](_)).use {
      _.readFile(filePath(name))
    }

  val runIO: IO[Assertion] => Assertion = _.unsafeRunSync()

  val executionContextResource = ExecutionContexts.unboundedCached[IO]
}
