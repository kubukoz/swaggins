package swaggins

import java.nio.file.{Path, Paths}

import cats.effect.{ContextShift, IO, Timer}
import org.scalatest.{Assertion, AsyncWordSpec, Matchers}
import swaggins.core.{ExecutionContexts, Parser}

import scala.concurrent.{ExecutionContextExecutorService, Future}

trait BaseTest extends AsyncWordSpec with Matchers {
  implicit val cs: ContextShift[IO] = IO.contextShift(executionContext)
  implicit val timer: Timer[IO]     = IO.timer(executionContext)

  def filePath(name: String): Path = {
    Paths.get(getClass.getResource(name).toURI)
  }

  def fileContent(name: String): IO[String] =
    ExecutionContexts.unboundedCached[IO].use {
      Parser.readFile[IO](_, filePath(name))
    }

  val runIO: IO[Assertion] => Future[Assertion] = _.unsafeToFuture()

  val runIOWithEc
    : (ExecutionContextExecutorService => IO[Assertion]) => Future[Assertion] =
    runIO.compose(ExecutionContexts.unboundedCached[IO].use[Assertion](_))
}
