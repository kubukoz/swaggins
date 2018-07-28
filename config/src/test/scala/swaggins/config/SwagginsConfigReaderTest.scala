package swaggins.config

import cats.implicits._
import monix.eval.Coeval
import swaggins.BaseTest
import swaggins.config.model.SwagginsConfig
import swaggins.config.model.code._
import swaggins.config.model.shared.SourceIdentifier
import swaggins.config.model.sources._

import scala.collection.immutable.SortedMap

class SwagginsConfigReaderTest extends BaseTest {
  "reader" should {
    val reader: SwagginsConfigReader[Coeval] =
      new SwagginsConfigReader[Coeval]

    val parsed = SwagginsConfig(
      Code(
        SortedMap(
          SourceIdentifier("kubukoz/hyze-spec") -> SourceSpecs(SortedMap(
            SpecIdentifier("transaction", "0.0.1") -> SpecGenerators(
              SortedMap(GeneratorKey("http4s-server") -> GeneratorConfig(
                          "src/main/scala"),
                        GeneratorKey("http4s-client") -> GeneratorConfig(
                          "src/test/scala"))),
            SpecIdentifier("account", "0.0.2") -> SpecGenerators(
              SortedMap(GeneratorKey("http4s-client") -> GeneratorConfig(
                "src/test/scala")))
          )))),
      Sources(
        SortedMap(
          SourceIdentifier("kubukoz/hyze-spec") -> List(
            SourceUri("fs", "../hyze-spec"),
            SourceUri("gh", "kubukoz/hyze-spec"))))
    )

    val path = filePath("/swaggins.json")

    "parse the first example" in {
      reader.read(path).value shouldBe parsed
    }

    "parse and acquire" in {
      reader.get(path).value shouldBe parsed
    }

    "acquire the parsed value" in {
      reader.get(path).value shouldBe parsed
    }
  }
}
