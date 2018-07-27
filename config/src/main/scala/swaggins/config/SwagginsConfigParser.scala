package swaggins.config
import java.nio.file.Path

import cats.effect.Sync

class SwagginsConfigParser[F[_]: Sync] {

  def parse(path: Path): F[SwagginsConfig] =
    /*Parsing.json.parseFile[F, SwagginsConfig](path)*/ Sync[F].pure(SwagginsConfig())
}

case class SwagginsConfig()
