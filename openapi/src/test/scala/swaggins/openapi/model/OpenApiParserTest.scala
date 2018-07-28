package swaggins.openapi.model

import cats.data.{NonEmptyList, NonEmptyMap, NonEmptySet}
import monix.eval.Coeval
import swaggins.BaseTest
import swaggins.openapi.OpenApiParser
import swaggins.openapi.experimental.Experiment
import swaggins.openapi.model.components._
import swaggins.openapi.model.paths.HttpMethod._
import swaggins.openapi.model.paths._
import swaggins.openapi.model.shared._

class OpenApiParserTest extends BaseTest {
  "the parser" should {
    "parse the sample spec" in {
      val parser: OpenApiParser[Coeval] = new OpenApiParser[Coeval]

      val getBalanceOperation = {
        val jsonResponse = NonEmptyMap.of(
          ContentType("application/json") -> MediaType(Some(Left(Reference(
            ReferenceString("#/components/schemas/account-balances"))))))

        Operation(
          Responses(
            NonEmptyMap.of(StatusCode(200) -> Response(Some(jsonResponse)))))
      }

      val balancePath = PathItem(NonEmptyMap.of(Get -> getBalanceOperation))

      val postTransactionsOperation = Operation(
        Responses(
          NonEmptyMap.of(
            StatusCode(200) -> Response(
              Some(
                NonEmptyMap.of(
                  ContentType("application/json") -> MediaType(
                    Some(Right(NumberSchema)))
                )
              )))))

      val transactionsPath =
        PathItem(NonEmptyMap.of(Post -> postTransactionsOperation))

      val paths = Paths(
        NonEmptySet.of(Path("/balance", balancePath),
                       Path("/transactions", transactionsPath)))

      val balanceNodeSchema = ObjectSchema(
        Some(NonEmptyList.of(SchemaName("id"), SchemaName("name"))),
        NonEmptyList.of(
          Property(SchemaName("id"), Right(NumberSchema)),
          Property(SchemaName("name"), Right(StringSchema)),
          Property(
            SchemaName("balance"),
            Left(Reference(ReferenceString("#/components/schemas/money"))))
        )
      )

      val balanceTreeSchema = ObjectSchema(
        Some(NonEmptyList.of(SchemaName("value"), SchemaName("children"))),
        NonEmptyList.of(
          Property(
            SchemaName("value"),
            Left(
              Reference(
                ReferenceString("#/components/schemas/account-balance-node")))),
          Property(
            SchemaName("children"),
            Right(ArraySchema(Left(Reference(
              ReferenceString("#/components/schemas/account-balance-tree"))))))
        )
      )

      val balanceListSchema = ObjectSchema(
        Some(NonEmptyList.one(SchemaName("children"))),
        NonEmptyList.of(
          Property(
            SchemaName("children"),
            Right(ArraySchema(Left(Reference(
              ReferenceString("#/components/schemas/account-balance-tree"))))))
        )
      )

      val components = Components(
        NonEmptyMap.of(
          SchemaName("account-balance-node") -> Right(balanceNodeSchema),
          SchemaName("account-balance-tree") -> Right(balanceTreeSchema),
          SchemaName("account-balances")     -> Right(balanceListSchema),
          SchemaName("money")                -> Right(NumberSchema)
        ))

      val expected =
        OpenAPI("3.0.1", Info("1.0.0", "My example project"), paths, components)

      parser.parse(filePath("/parsing-works.yml")).value shouldBe expected
      Experiment.gen(expected).foreach(println)
    }
  }
}
