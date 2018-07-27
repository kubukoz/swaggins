package com.kubukoz.swaggins.openapi.model
import java.nio.file
import java.nio.file.{Paths => NioPaths}

import monix.eval.Coeval
import org.scalatest.{Matchers, WordSpec}

class OpenApiParsingTest extends WordSpec with Matchers {

  "the parser" should {
    "parse the sample spec" in {
      import OpenApiParser.parse

      parse[Coeval](filePath("/parsing-works.yml")).value
    }
  }
  private def filePath(name: String): file.Path = {
    NioPaths.get(getClass.getResource(name).toURI)
  }
}
