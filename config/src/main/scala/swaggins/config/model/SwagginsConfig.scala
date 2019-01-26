package swaggins.config.model

import scalaz.deriving
import io.circe.Decoder
import swaggins.config.model.code.Code
import swaggins.config.model.sources.Sources

@deriving(Decoder)
final case class SwagginsConfig(code: Code, sources: Sources)
