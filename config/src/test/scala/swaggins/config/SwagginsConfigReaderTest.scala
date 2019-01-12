package swaggins.config

import cats.data._
import cats.effect.{ContextShift, IO, Resource, Sync}
import cats.implicits._
import cats.temp.par.{parToParallel => _}
import io.circe.DecodingFailure
import swaggins.BaseTest
import swaggins.config.error.ConfigValidationError
import swaggins.config.error.ConfigValidationError.UnknownSources
import swaggins.config.model.SwagginsConfig
import swaggins.config.model.code._
import swaggins.config.model.shared.SourceIdentifier
import swaggins.config.model.sources._
import swaggins.core.FileReader

class SwagginsConfigReaderTest extends BaseTest {

  def fileReader[F[_]: Sync: ContextShift]: Resource[IO, FileReader[F]] =
    executionContextResource.map(ec => FileReader.make[F](ec))

  "reader/validator" should {
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

    "parse the first example" in runIO {
      fileReader[IO].use { implicit fr =>
        implicit val validator: SwagginsConfigValidator[IO] = _.pure[IO].void

        val reader: SwagginsConfigReader[IO] = SwagginsConfigReader.make
        reader.read(path).map(_ shouldBe parsed)
      }
    }

    "fail if a source's URIs are empty" in runIO {
      fileReader[IO].use { implicit fr =>
        implicit val validator: SwagginsConfigValidator[IO] = _.pure[IO].void

        val reader: SwagginsConfigReader[IO] = SwagginsConfigReader.make

        reader.read(filePath("/empty-source-uris.json")).attempt.map {
          case Left(e)  => e shouldBe a[DecodingFailure]
          case Right(_) => fail()
        }
      }
    }

    "validate the first example" in {
      val validator: SwagginsConfigValidator[ConfigValidationError.EitherNel] =
        SwagginsConfigValidator.make[ConfigValidationError.EitherNel]

      validator.validateConfig(parsed) shouldBe Right(())
    }

    "not validate if a source isn't defined" in {
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

      val validator: SwagginsConfigValidator[ConfigValidationError.EitherNel] =
        SwagginsConfigValidator.make[ConfigValidationError.EitherNel]

      validator.validateConfig(invalid) match {
        case Left(e) =>
          e should have size 1
          e.head.asInstanceOf[UnknownSources].source shouldBe SourceIdentifier(
            "kubukoz/hyze-spec2")
        case Right(_) => fail()
      }
    }
  }
}
