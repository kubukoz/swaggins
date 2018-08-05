package swaggins.generator
import monix.eval.Coeval
import swaggins.BaseTest
import swaggins.openapi.model.OpenApiParserTest
import cats.implicits._
import org.scalatest.Succeeded

class ScalaModelGeneratorTest extends BaseTest {
  "generator" when {
    "the sample file is given" should {
      "generate valid code" in {
        val generator: Generator[Coeval] = new ScalaCaseClassGenerator[Coeval]

        val actual =
          generator
            .generate(OpenApiParserTest.expected.full)
            .compile
            .toList
            .value

        val expected = List(
          GeneratedFile("models.scala",
                        fileContent("/expected-scala-models.scala"))
        )

//        println(expected.head.content)
        compare(actual, expected)
      }
    }
  }

  def compare(actual: List[GeneratedFile],
              expected: List[GeneratedFile]): Unit = {
    actual should have size expected.size.toLong

    val actualByName: Map[String, GeneratedFile] =
      actual.groupByNel(_.name).mapValues(_.head)

    expected.foreach { expectedFile =>
      actualByName.get(expectedFile.name) match {
        case Some(actualFile) if actualFile == expectedFile => Succeeded
        case Some(actualFile) =>
          fail(
            show"""${expectedFile.name}'s content wasn't equal to expected. Got:
          |${actualFile.content}""".stripMargin)
        case None => fail(s"${expectedFile.name}: file not found")
      }
    }
  }
}
