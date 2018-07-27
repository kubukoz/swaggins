package swaggins.app

import cats.{Applicative, Apply}
import swaggins.config.SwagginsConfig
import swaggins.fetch.SpecSource
import swaggins.generator.{GeneratedFile, Generator}
import swaggins.openapi.model.OpenAPI

class App[F[_]: Applicative](specSource: SpecSource[F]) {

  def generate(config: SwagginsConfig,
               spec: OpenAPI): fs2.Stream[F, GeneratedFile] = {
    val generators: List[Generator[F]] = selectGenerators(config)

    generate(spec, generators)
  }

  def selectGenerators(config: SwagginsConfig): List[Generator[F]] = ???

  def generate(spec: OpenAPI,
               generators: List[Generator[F]]): fs2.Stream[F, GeneratedFile] = {
    val tree = dependencyTree(spec)

    Apply[fs2.Stream[F, ?]]
      .map2(fs2.Stream.emits(generators), tree)(_.generate(_))
      .flatMap(identity) //todo join?
  }

  final def dependencyTree(spec: OpenAPI): fs2.Stream[F, OpenAPI] =
    specSource.directDependencies(spec).flatMap(dependencyTree)
}
