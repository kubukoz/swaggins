package swaggins.openapi.model

import cats.data
import cats.data.{NonEmptyList, NonEmptyMap, NonEmptySet}
import monix.eval.Coeval
import swaggins.BaseTest
import swaggins.openapi.OpenApiParser
import swaggins.openapi.experimental.Experiment
import swaggins.openapi.model.components._
import swaggins.openapi.model.paths.HttpMethod._
import swaggins.openapi.model.paths._
import swaggins.openapi.model.shared._
import cats.implicits._

class OpenApiParserTest extends BaseTest {

  def componentRef(name: String): Reference =
    Reference(ReferenceString(show"#/components/schemas/$name"))

  "the parser" should {
    val parser: OpenApiParser[Coeval] = new OpenApiParser[Coeval]

    "parse the sample spec" in {
      val getBalanceOperation = {
        val jsonResponse = NonEmptyMap.of(
          ContentType("application/json") -> MediaType(
            Some(Left(componentRef("account-balances")))))

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
          Property(SchemaName("balance"), Left(componentRef("money")))
        )
      )

      val balanceTreeSchema = ObjectSchema(
        Some(NonEmptyList.of(SchemaName("value"), SchemaName("children"))),
        NonEmptyList.of(
          Property(SchemaName("value"),
                   Left(componentRef("account-balance-node"))),
          Property(
            SchemaName("children"),
            Right(ArraySchema(Left(componentRef("account-balance-tree")))))
        )
      )

      val balanceListSchema = ObjectSchema(
        Some(NonEmptyList.one(SchemaName("children"))),
        NonEmptyList.of(
          Property(
            SchemaName("children"),
            Right(ArraySchema(Left(componentRef("account-balance-tree")))))
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

    "parse the coproducts spec" in {
      val getPetSchema = ArraySchema(
        Right(
          CompositeSchema(data.NonEmptyList.of(Left(componentRef("pet")),
                                               Left(componentRef("strnum"))),
                          CompositeSchemaKind.AnyOf)))

      val getPetOperation = Operation(
        Responses(
          NonEmptyMap.of(
            StatusCode(200) -> Response(Some(NonEmptyMap.of(ContentType(
              "application/json") -> MediaType(Some(Right(getPetSchema)))))))))

      val paths = Paths(
        NonEmptySet.of(
          Path("/pet", PathItem(NonEmptyMap.of(Get -> getPetOperation)))))

      val strNumSchema = CompositeSchema(
        NonEmptyList.of(Right(StringSchema), Right(NumberSchema)),
        CompositeSchemaKind.OneOf)

      val petSchema = CompositeSchema(
        NonEmptyList.of(Left(componentRef("cat")), Left(componentRef("dog"))),
        CompositeSchemaKind.OneOf)

      val catSchema = ObjectSchema(
        None,
        NonEmptyList.of(
          Property(SchemaName("huntingSkill"), Right(StringSchema))))

      val dogSchema =
        CompositeSchema(NonEmptyList.of(Left(componentRef("husky")),
                                        Left(componentRef("york"))),
                        CompositeSchemaKind.OneOf)

      val huskySchema = ObjectSchema(
        None,
        NonEmptyList.of(Property(SchemaName("woof"), Right(StringSchema))))

      val yorkSchema = huskySchema

      val components = Components(
        NonEmptyMap.of(
          SchemaName("strnum") -> Right(strNumSchema),
          SchemaName("pet")    -> Right(petSchema),
          SchemaName("cat")    -> Right(catSchema),
          SchemaName("dog2")   -> Left(componentRef("dog")),
          SchemaName("dog")    -> Right(dogSchema),
          SchemaName("husky")  -> Right(huskySchema),
          SchemaName("york")   -> Right(yorkSchema)
        ))

      val expected =
        OpenAPI("3.0.1", Info("1.0.0", "Swagger Petstore"), paths, components)

      parser.parse(filePath("/coproducts.yml")).value shouldBe expected
    }
  }
}
