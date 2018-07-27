package com.kubukoz.swaggins.openapi.model
import java.nio.file
import java.nio.file.{Paths => NioPaths}

import monix.eval.Coeval
import org.scalatest.{Matchers, WordSpec}

class OpenApiParsingTest extends WordSpec with Matchers {

  "the parser" should {
    "parse the sample spec" in {
      import OpenApiParser.parse

      parse[Coeval](filePath("/parsing-works.yml")).value shouldBe OpenAPI(
        "3.0.1",
        Info(
          "1.0.0",
          "My example project"
        ),
        Paths(
          Set(
            Path("/balance", PathItem(Map(HttpMethod.Get             -> Operation()))),
            Path("/auth/login", PathItem(Map(HttpMethod.Post         -> Operation()))),
            Path("/register", PathItem(Map(HttpMethod.Post           -> Operation()))),
            Path("/auth/logout", PathItem(Map(HttpMethod.Post        -> Operation()))),
            Path("/auth/whomst", PathItem(Map(HttpMethod.Get         -> Operation()))),
            Path("/auth/token/refresh", PathItem(Map(HttpMethod.Post -> Operation())))
          ))
      )
    }
  }
  private def filePath(name: String): file.Path = {
    NioPaths.get(getClass.getResource(name).toURI)
  }
}
