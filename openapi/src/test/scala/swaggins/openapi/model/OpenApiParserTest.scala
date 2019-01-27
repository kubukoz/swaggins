package swaggins.openapi.model

import cats.data
import cats.data.{NonEmptyList, NonEmptyMap, NonEmptySet}
import cats.effect.IO
import cats.implicits._
import swaggins.BaseTest
import swaggins.core.{ExecutionContexts, FileReader}
import swaggins.openapi.OpenApiParser
import swaggins.openapi.model.OpenApiParserTest.expected
import swaggins.openapi.model.components._
import swaggins.openapi.model.paths.HttpMethod._
import swaggins.openapi.model.paths._
import swaggins.openapi.model.shared._

object TestImplicits

class OpenApiParserTest extends BaseTest {

  val parserResource =
    ExecutionContexts.unboundedCached[IO].map { ec =>
      implicit val fr = FileReader.make[IO](ec)
      OpenApiParser.make
    }

  "the parser" should {
    "parse the sample spec" in runIO {
      parserResource.use {
        _.parse(filePath("/parsing-works.yml")).map(_ shouldBe expected.full)
      }
    }

    "parse the coproducts spec" in runIO {
      parserResource.use {
        _.parse(filePath("/coproducts.yml")).map(_ shouldBe expected.coproducts)
      }
    }
  }
}

object OpenApiParserTest {

  def componentRef(name: String): RefOrSchema =
    RefOrSchema.Reference(ReferenceRef.ComponentRef(SchemaName(name)))

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
        Content(MediaType(Some(componentRef("account-balances"))))

      Operation(
        Responses(
          NonEmptyMap.of(StatusCode(200) -> Response(Some(balanceContent)))))
    }

    val balancePath = PathItem(NonEmptyMap.of(Get -> getBalanceOperation))

    val postTransactionsOperation = Operation(
      Responses(
        NonEmptyMap.of(StatusCode(200) -> Response(Some(
          Content(MediaType(Some(RefOrSchema.InlineSchema(NumberSchema(None)))))
        )))))

    val transactionsPath =
      PathItem(NonEmptyMap.of(Post -> postTransactionsOperation))

    val paths = Paths(
      NonEmptySet.of(Path("/balance", balancePath),
                     Path("/transactions", transactionsPath)))

    val balanceNodeSchema = ObjectSchema(
      Some(NonEmptySet.of(PropertyName("id"), PropertyName("name"))),
      NonEmptyList.of(
        Property(PropertyName("id"),
                 RefOrSchema.InlineSchema(NumberSchema(None))),
        Property(PropertyName("name"),
                 RefOrSchema.InlineSchema(StringSchema(None))),
        Property(PropertyName("balance"), componentRef("money"))
      )
    )

    val balanceTreeSchema = ObjectSchema(
      Some(NonEmptySet.of(PropertyName("value"), PropertyName("children"))),
      NonEmptyList.of(
        Property(PropertyName("value"), componentRef("account-balance-node")),
        Property(PropertyName("children"),
                 RefOrSchema.InlineSchema(
                   ArraySchema(componentRef("account-balance-tree"))))
      )
    )

    val balanceListSchema = ObjectSchema(
      Some(NonEmptySet.one(PropertyName("children"))),
      NonEmptyList.of(
        Property(PropertyName("children"),
                 RefOrSchema.InlineSchema(
                   ArraySchema(componentRef("account-balance-tree"))))
      )
    )

    val components = Components(
      NonEmptyMap.of(
        SchemaName("account-balance-node") -> RefOrSchema.InlineSchema(
          balanceNodeSchema),
        SchemaName("account-balance-tree") -> RefOrSchema.InlineSchema(
          balanceTreeSchema),
        SchemaName("account-balances") -> RefOrSchema.InlineSchema(
          balanceListSchema),
        SchemaName("money") -> RefOrSchema.InlineSchema(NumberSchema(None))
      ))
  }

  private object coproductsParts {

    val getPetSchema = ArraySchema(
      RefOrSchema.InlineSchema(
        CompositeSchema(
          data.NonEmptyList.of(componentRef("pet"), componentRef("strnum")),
          CompositeSchemaKind.AnyOf,
          None)))

    val petsContent = Content(
      MediaType(Some(RefOrSchema.InlineSchema(getPetSchema))))

    val getPetOperation = Operation(
      Responses(NonEmptyMap.of(StatusCode(200) -> Response(Some(petsContent)))))

    val paths = Paths(
      NonEmptySet.of(
        Path("/pet", PathItem(NonEmptyMap.of(Get -> getPetOperation)))))

    val strNumSchema = CompositeSchema(
      NonEmptyList.of(RefOrSchema.InlineSchema(StringSchema(None)),
                      RefOrSchema.InlineSchema(NumberSchema(None))),
      CompositeSchemaKind.OneOf,
      None)

    val petSchema = CompositeSchema(
      NonEmptyList.of(componentRef("cat"), componentRef("dog")),
      CompositeSchemaKind.OneOf,
      Some(
        Discriminator(Some(PropertyName("petType")),
                      Some(NonEmptyMap.of("cat_type" -> PropertyName("cat")))))
    )

    val huntingSkillSchema = StringSchema(
      Some(NonEmptySet.of("clueless", "lazy", "adventurous", "aggressive")))

    val catSchema = ObjectSchema(
      None,
      NonEmptyList.of(
        Property(PropertyName("hunting-skill"),
                 RefOrSchema.InlineSchema(huntingSkillSchema))))

    val anonymousDogSchema = ObjectSchema(
      Some(NonEmptySet.of(PropertyName("name"), PropertyName("age"))),
      NonEmptyList.of(
        Property(PropertyName("name"),
                 RefOrSchema.InlineSchema(StringSchema(None))),
        Property(PropertyName("age"),
                 RefOrSchema.InlineSchema(NumberSchema(None))),
        Property(PropertyName("gender"),
                 RefOrSchema.InlineSchema(
                   StringSchema(Some(NonEmptySet.of("male", "female")))))
      )
    )

    val dogSchema =
      CompositeSchema(
        NonEmptyList.of(
          componentRef("husky"),
          componentRef("york"),
          RefOrSchema.InlineSchema(anonymousDogSchema)
        ),
        CompositeSchemaKind.OneOf,
        Some(Discriminator(Some(PropertyName("dogType")), None))
      )

    val huskySchema =
      ObjectSchema(None,
                   NonEmptyList.of(
                     Property(PropertyName("woof"),
                              RefOrSchema.InlineSchema(StringSchema(None)))))

    val yorkSchema = ObjectSchema(
      None,
      NonEmptyList.of(
        Property(
          SchemaName("doggo"),
          RefOrSchema.InlineSchema(
            ObjectSchema(Some(NonEmptySet.of(SchemaName("dog"))),
                         NonEmptyList.of(
                           Property(SchemaName("dog"), componentRef("dog")))))
        ))
    )

    val doggieSchema = CompositeSchema(
      NonEmptyList.of(
        RefOrSchema.InlineSchema(
          ObjectSchema(
            None,
            NonEmptyList.of(
              Property(SchemaName("name"),
                       RefOrSchema.InlineSchema(StringSchema(None))),
              Property(
                SchemaName("age"),
                RefOrSchema.InlineSchema(ObjectSchema(
                  None,
                  NonEmptyList.of(
                    Property(SchemaName("age"),
                             RefOrSchema.InlineSchema(NumberSchema(None))),
                    Property(SchemaName("wellness"),
                             RefOrSchema.InlineSchema(StringSchema(
                               Some(NonEmptySet.of("good", "bad")))))
                  )
                ))
              )
            )
          )),
        RefOrSchema.InlineSchema(
          StringSchema(Some(NonEmptySet.of("foo", "bar", "baz")))
        ),
        componentRef("york")
      ),
      CompositeSchemaKind.OneOf,
      None
    )

    val components = Components(
      NonEmptyMap.of(
        SchemaName("strnum") -> RefOrSchema.InlineSchema(strNumSchema),
        SchemaName("pet")    -> RefOrSchema.InlineSchema(petSchema),
        SchemaName("cat")    -> RefOrSchema.InlineSchema(catSchema),
        SchemaName("dog2")   -> componentRef("dog"),
        SchemaName("dog")    -> RefOrSchema.InlineSchema(dogSchema),
        SchemaName("husky")  -> RefOrSchema.InlineSchema(huskySchema),
        SchemaName("york")   -> RefOrSchema.InlineSchema(yorkSchema),
        SchemaName("doggie") -> RefOrSchema.InlineSchema(doggieSchema)
      ))
  }
}
