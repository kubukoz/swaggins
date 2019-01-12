package swaggins

import java.nio.file.{Path, Paths}

import cats.effect.{ContextShift, IO, Timer}
import cats.implicits._
import org.scalatest.{Assertion, AsyncWordSpec, Matchers}
import swaggins.core.{ExecutionContexts, FileReader}

import scala.concurrent.Future

trait BaseTest extends AsyncWordSpec with Matchers {
  implicit val cs: ContextShift[IO] = IO.contextShift(executionContext)
  implicit val timer: Timer[IO]     = IO.timer(executionContext)

  type With[F[_[_]], G[_]] = (F[G] => G[Assertion]) => G[Assertion]

  def filePath(name: String): Path = {
    Paths.get(getClass.getResource(name).toURI)
  }

  def fileContent(name: String): IO[String] =
    ExecutionContexts.unboundedCached[IO].map(FileReader.make[IO](_)).use {
      _.readFile(filePath(name))
    }

  val runIO: IO[Assertion] => Future[Assertion] = _.unsafeToFuture()

  val executionContextResource = ExecutionContexts.unboundedCached[IO]
}
