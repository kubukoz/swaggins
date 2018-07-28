package swaggins.openapi.model

import monix.eval.Coeval
import swaggins.BaseTest
import cats.data.{NonEmptyMap, NonEmptySet}

class OpenApiParserTest extends BaseTest {
  "the parser" should {
    "parse the sample spec" in {
      val parser: OpenApiParser[Coeval] = new OpenApiParser[Coeval]

      parser.parse(filePath("/parsing-works.yml")).value shouldBe OpenAPI(
        "3.0.1",
        Info(
          "1.0.0",
          "My example project"
        ),
        Paths(
          NonEmptySet.of(
            Path("/balance",
                 PathItem(NonEmptyMap.one(HttpMethod.Get, Operation()))),
            Path("/transactions",
                 PathItem(NonEmptyMap.one(HttpMethod.Post, Operation())))
          ))
      )
    }
  }
}
