package swaggins.generator

import cats.data.ReaderT
import cats.effect.IO
import cats.implicits._
import org.scalatest.Assertion
import swaggins.BaseTest
import swaggins.generator.convert.Packages
import swaggins.openapi.model.OpenApiParserTest
import cats.mtl.implicits._

class ScalaModelGeneratorTest extends BaseTest {
  type F[A] = ReaderT[IO, Packages, A]

  "generator" when {
    /*"the sample file is given" should {
      "generate valid code" in runIO {
        val generator: Generator[F] = new ScalaCaseClassGenerator[F]

        val actual =
          generator
            .generate(OpenApiParserTest.expected.full)
            .compile
            .toList
            .run(Packages.empty)

        val expected = fileContent("/expected-scala-models.scala")
          .map(GeneratedFile("models.scala", _))
          .map(List(_))

        (actual, expected).mapN(compare).flatten
      }
    }
*/
    "the coproducts file is given" should {
      "generate valid code" in runIO {
        val generator: Generator[F] = new ScalaCaseClassGenerator[F]

        val actual =
          generator
            .generate(OpenApiParserTest.expected.coproducts)
            .compile
            .toList
            .run(Packages.empty)

        val expected = fileContent("/expected-coproducts-scala-models.scala")
          .map(GeneratedFile("models.scala", _))
          .map(List(_))

        (actual, expected).mapN(compare).flatten
      }
    }
  }

  def compare(actual: List[GeneratedFile],
              expected: List[GeneratedFile]): IO[Assertion] = {
    val actualByName: Map[String, GeneratedFile] =
      actual.groupByNel(_.name).mapValues(_.head)

    IO(actual should have size expected.size.toLong) *>
      expected.traverse { expectedFile =>
        actualByName.get(expectedFile.name) match {
          case Some(actualFile) if actualFile eqv expectedFile =>
            succeed.pure[IO]
          case Some(actualFile) =>
            IO(fail(
              show"${expectedFile.name}'s content wasn't equal to expected. Got:\n" + actualFile.content + "\n"))

          case None => IO(fail(s"${expectedFile.name}: file not found"))
        }
      }.map {
        case Nil => succeed
        case nem => nem.last
      }
  }
}
