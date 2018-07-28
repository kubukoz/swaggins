package swaggins.config

import cats.data.NonEmptyList
import cats.data._
import io.circe.DecodingFailure
import monix.eval.Coeval
import swaggins.BaseTest
import swaggins.config.error.UnknownSourcesException
import swaggins.config.model.SwagginsConfig
import swaggins.config.model.code._
import swaggins.config.model.shared.SourceIdentifier
import swaggins.config.model.sources._

class SwagginsConfigReaderTest extends BaseTest {
  "reader" should {
    val reader: SwagginsConfigReader[Coeval] =
      new SwagginsConfigReader[Coeval]

    val parsed = SwagginsConfig(
      Code(
        NonEmptyMap.of(
          SourceIdentifier("kubukoz/hyze-spec") -> SourceSpecs(NonEmptyMap.of(
            SpecIdentifier("transaction", "0.0.1") -> SpecGenerators(
              NonEmptyMap.of(GeneratorKey("http4s-server") -> GeneratorConfig(
                               "src/main/scala"),
                             GeneratorKey("http4s-client") -> GeneratorConfig(
                               "src/test/scala"))),
            SpecIdentifier("account", "0.0.2") -> SpecGenerators(
              NonEmptyMap.of(GeneratorKey("http4s-client") -> GeneratorConfig(
                "src/test/scala")))
          )))),
      Sources(
        NonEmptyMap.of(
          SourceIdentifier("kubukoz/hyze-spec") -> NonEmptyList.of(
            SourceUri(SourceScheme.Filesystem, "../hyze-spec"),
            SourceUri(SourceScheme.Github, "kubukoz/hyze-spec"))))
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
          NonEmptyMap.of(
            SourceIdentifier("kubukoz/hyze-spec2") -> SourceSpecs(
              NonEmptyMap.one(
                SpecIdentifier("transaction", "0.0.1"),
                SpecGenerators(
                  NonEmptyMap.one(GeneratorKey("http4s-server"),
                                  GeneratorConfig("src/main/scala"))
                ))))),
        Sources(
          NonEmptyMap.of(SourceIdentifier("kubukoz/hyze-spec") -> NonEmptyList
            .one(SourceUri(SourceScheme.Filesystem, "../hyze-spec"))))
      )

      reader
        .get(invalid)
        .failed
        .value
        .asInstanceOf[UnknownSourcesException]
        .sources shouldBe NonEmptyList.of(
        SourceIdentifier("kubukoz/hyze-spec2"))

    }
  }
}
