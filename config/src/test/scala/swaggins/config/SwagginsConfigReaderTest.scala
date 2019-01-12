package swaggins.config

import cats.data.{NonEmptyList, _}
import cats.effect.IO
import io.circe.DecodingFailure
import swaggins.BaseTest
import swaggins.config.error.UnknownSourcesException
import swaggins.config.model.SwagginsConfig
import swaggins.config.model.code._
import swaggins.config.model.shared.SourceIdentifier
import swaggins.config.model.sources._

class SwagginsConfigReaderTest extends BaseTest {
  "reader" should {
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

    "parse the first example" in runIOWithEc { ec =>
      SwagginsConfigReader.make[IO](ec).read(path).map(_ shouldBe parsed)
    }

    "fail if a source's URIs are empty" in runIOWithEc { ec =>
      SwagginsConfigReader
        .make[IO](ec)
        .read(filePath("/empty-source-uris.json"))
        .attempt
        .map {
          case Left(e)  => e shouldBe a[DecodingFailure]
          case Right(_) => fail()
        }
    }

    "get the first example" in runIOWithEc { ec =>
      SwagginsConfigReader.make[IO](ec).get(parsed).map(_ shouldBe parsed)
    }

    "read+get the first example" in runIOWithEc { ec =>
      val reader: SwagginsConfigReader[IO] =
        SwagginsConfigReader.make[IO](ec)

      reader.read(path).flatMap(reader.get).map(_ shouldBe parsed)
    }

    "not get if a source isn't defined" in runIOWithEc { ec =>
      val invalid = SwagginsConfig(
        Code(
          NonEmptyMap.of(SourceIdentifier("kubukoz/hyze-spec2") -> SourceSpecs(
            NonEmptyMap.one(SpecIdentifier("transaction", "0.0.1"),
                            SpecGenerators(
                              NonEmptyMap.one(GeneratorKey("http4s-server"),
                                              GeneratorConfig("src/main/scala"))
                            ))))),
        Sources(
          NonEmptyMap.of(SourceIdentifier("kubukoz/hyze-spec") -> NonEmptyList
            .one(SourceUri(SourceScheme.Filesystem, "../hyze-spec"))))
      )

      SwagginsConfigReader.make[IO](ec).get(invalid).attempt.map {
        case Left(e) =>
          e.asInstanceOf[UnknownSourcesException].sources shouldBe NonEmptySet
            .of(SourceIdentifier("kubukoz/hyze-spec2"))
        case Right(_) => fail()
      }
    }
  }
}
