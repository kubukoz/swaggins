package swaggins.config.model
import io.circe.generic.JsonCodec
import swaggins.config.model.code.Code
import swaggins.config.model.sources.Sources

@JsonCodec(decodeOnly = true)
case class SwagginsConfig(code: Code, sources: Sources)
