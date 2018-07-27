package swaggins.app

import cats.Applicative
import swaggins.config.SwagginsConfig
import swaggins.fetch.SpecSource
import swaggins.generator.{GeneratedFile, Generator}

class App[F[_]: Applicative](specSource: SpecSource[F]) {
  val availableGenerators: List[Generator[F]] = ???

  def generate(config: SwagginsConfig): fs2.Stream[F, GeneratedFile] = ???
}
