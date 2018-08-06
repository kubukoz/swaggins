package swaggins.openapi.model

import cats.data
import cats.data.{NonEmptyList, NonEmptyMap, NonEmptySet}
import monix.eval.Coeval
import swaggins.BaseTest
import swaggins.openapi.OpenApiParser
import swaggins.openapi.model.components._
import swaggins.openapi.model.paths.HttpMethod._
import swaggins.openapi.model.paths._
import swaggins.openapi.model.shared._
import cats.implicits._
import swaggins.openapi.model.OpenApiParserTest.expected

class OpenApiParserTest extends BaseTest {
  "the parser" should {
    val parser: OpenApiParser[Coeval] = new OpenApiParser[Coeval]

    "parse the sample spec" in {
      parser.parse(filePath("/parsing-works.yml")).value shouldBe expected.full
    }

    "parse the coproducts spec" in {
      parser
        .parse(filePath("/coproducts.yml"))
        .value shouldBe expected.coproducts
    }
  }
}

object OpenApiParserTest {

  def componentRef(name: String): Reference =
    Reference(ReferenceRef.ComponentRef(SchemaName(name)))

  object expected {

    val full: OpenAPI = {
      import fullParts._
      OpenAPI("3.0.1", Info("1.0.0", "My example project"), paths, components)
    }

    val coproducts: OpenAPI = {
      import coproductsParts._
      OpenAPI("3.0.1", Info("1.0.0", "Swagger Petstore"), paths, components)
    }
  }

  private object fullParts {

    val getBalanceOperation = {
      val balanceContent =
        Content(MediaType(Some(Left(componentRef("account-balances")))))

      Operation(
        Responses(
          NonEmptyMap.of(StatusCode(200) -> Response(Some(balanceContent)))))
    }

    val balancePath = PathItem(NonEmptyMap.of(Get -> getBalanceOperation))

    val postTransactionsOperation = Operation(
      Responses(
        NonEmptyMap.of(
          StatusCode(200) -> Response(Some(
            Content(MediaType(Some(Right(NumberSchema(None)))))
          )))))

    val transactionsPath =
      PathItem(NonEmptyMap.of(Post -> postTransactionsOperation))

    val paths = Paths(
      NonEmptySet.of(Path("/balance", balancePath),
                     Path("/transactions", transactionsPath)))

    val balanceNodeSchema = ObjectSchema(
      Some(NonEmptySet.of(SchemaName("id"), SchemaName("name"))),
      NonEmptyList.of(
        Property(SchemaName("id"), Right(NumberSchema(None))),
        Property(SchemaName("name"), Right(StringSchema(None))),
        Property(SchemaName("balance"), Left(componentRef("money")))
      )
    )

    val balanceTreeSchema = ObjectSchema(
      Some(NonEmptySet.of(SchemaName("value"), SchemaName("children"))),
      NonEmptyList.of(
        Property(SchemaName("value"),
                 Left(componentRef("account-balance-node"))),
        Property(SchemaName("children"),
                 Right(ArraySchema(Left(componentRef("account-balance-tree")))))
      )
    )

    val balanceListSchema = ObjectSchema(
      Some(NonEmptySet.one(SchemaName("children"))),
      NonEmptyList.of(
        Property(SchemaName("children"),
                 Right(ArraySchema(Left(componentRef("account-balance-tree")))))
      )
    )

    val components = Components(
      NonEmptyMap.of(
        SchemaName("account-balance-node") -> Right(balanceNodeSchema),
        SchemaName("account-balance-tree") -> Right(balanceTreeSchema),
        SchemaName("account-balances")     -> Right(balanceListSchema),
        SchemaName("money")                -> Right(NumberSchema(None))
      ))
  }

  private object coproductsParts {

    val getPetSchema = ArraySchema(
      Right(
        CompositeSchema(data.NonEmptyList.of(Left(componentRef("pet")),
                                             Left(componentRef("strnum"))),
                        CompositeSchemaKind.AnyOf,
                        None)))

    val petsContent = Content(MediaType(Some(Right(getPetSchema))))

    val getPetOperation = Operation(
      Responses(NonEmptyMap.of(StatusCode(200) -> Response(Some(petsContent)))))

    val paths = Paths(
      NonEmptySet.of(
        Path("/pet", PathItem(NonEmptyMap.of(Get -> getPetOperation)))))

    val strNumSchema = CompositeSchema(
      NonEmptyList.of(Right(StringSchema(None)), Right(NumberSchema(None))),
      CompositeSchemaKind.OneOf,
      None)

    val petSchema = CompositeSchema(
      NonEmptyList.of(Left(componentRef("cat")), Left(componentRef("dog"))),
      CompositeSchemaKind.OneOf,
      Some(
        Discriminator(Some(SchemaName("petType")),
                      Some(NonEmptyMap.of("cat_type" -> SchemaName("cat")))))
    )

    val huntingSkillSchema = StringSchema(
      Some(NonEmptySet.of("clueless", "lazy", "adventurous", "aggressive")))

    val catSchema = ObjectSchema(
      None,
      NonEmptyList.of(
        Property(SchemaName("hunting-skill"), Right(huntingSkillSchema))))

    val dogSchema =
      CompositeSchema(
        NonEmptyList.of(Left(componentRef("husky")),
                        Left(componentRef("york"))),
        CompositeSchemaKind.OneOf,
        Some(Discriminator(Some(SchemaName("dogType")), None))
      )

    val huskySchema =
      ObjectSchema(None,
                   NonEmptyList.of(
                     Property(SchemaName("woof"), Right(StringSchema(None)))))

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
  }
}
