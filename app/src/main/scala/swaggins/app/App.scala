package swaggins.app

import cats.Applicative
import swaggins.config.SwagginsConfig
import swaggins.generator.{GeneratedFile, Generator}
import swaggins.openapi.model.OpenAPI

class App[F[_]: Applicative] {

  def generate(config: SwagginsConfig, spec: OpenAPI): fs2.Stream[F, GeneratedFile] = {
    val generators: List[Generator[F]] = selectGenerators(config)

    fs2.Stream.emits(generators).flatMap(_.generate(spec))
  }

  def selectGenerators(config: SwagginsConfig): List[Generator[F]] = ???
}
