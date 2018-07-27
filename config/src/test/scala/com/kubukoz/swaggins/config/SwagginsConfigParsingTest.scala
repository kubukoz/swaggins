package com.kubukoz.swaggins.config
import com.kubukoz.swaggins.BaseTest
import monix.eval.Coeval

class SwagginsConfigParsingTest extends BaseTest {
  "parser" should {
    "parse the first example" in {
      val parser: SwagginsConfigParser[Coeval] =
        new SwagginsConfigParser[Coeval]

      parser.parse(filePath("/swaggins.json")).value shouldBe SwagginsConfig(
        Map(
          SpecSource(Some("gh"), "kubukoz") -> (()),
          SpecSource(None, "kumalg")        -> (())
        )
      )
    }
  }
}
