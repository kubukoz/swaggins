package swaggins.config

import cats.data.NonEmptyList
import cats.implicits._
import io.circe.DecodingFailure
import monix.eval.Coeval
import swaggins.BaseTest
import swaggins.config.error.UnknownSourcesException
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
          SourceIdentifier("kubukoz/hyze-spec") -> NonEmptyList.of(
            SourceUri("fs", "../hyze-spec"),
            SourceUri("gh", "kubukoz/hyze-spec"))))
    )

    val path = filePath("/swaggins.json")

    "parse the first example" in {
      reader.read(path).value shouldBe parsed
    }

    "fail if a source's URIs are empty" in {
      reader
        .read(filePath("/empty-source-uris.json"))
        .failed
        .value shouldBe a[DecodingFailure]
    }

    "get the first example" in {
      reader.get(parsed).value shouldBe parsed
    }

    "read+get the first example" in {
      reader.read(path).flatMap(reader.get).value shouldBe parsed
    }

    "not get if a source isn't defined" in {
      val invalid = SwagginsConfig(
        Code(
          SortedMap(SourceIdentifier("kubukoz/hyze-spec2") -> SourceSpecs(
            SortedMap.empty))),
        Sources(
          SortedMap(SourceIdentifier("kubukoz/hyze-spec") -> NonEmptyList.one(
            SourceUri("fs", "../hyze-spec"))))
      )

      reader
        .get(invalid)
        .failed
        .value
        .asInstanceOf[UnknownSourcesException]
        .sources shouldBe NonEmptyList.of(SourceIdentifier("kubukoz/hyze-spec2"))

    }
  }
}
