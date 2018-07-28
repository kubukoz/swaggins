package swaggins.openapi.model

import io.circe.generic.JsonCodec
import swaggins.openapi.model.components.Components
import swaggins.openapi.model.paths.Paths

@JsonCodec(decodeOnly = true)
case class OpenAPI(openapi: String,
                   info: Info,
                   paths: Paths,
                   components: Components)

@JsonCodec(decodeOnly = true)
case class Info(version: String, title: String)
