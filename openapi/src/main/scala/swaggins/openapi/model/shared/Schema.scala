package swaggins.openapi.model.shared

import cats.Show
import cats.data.{NonEmptyList, NonEmptyMap, NonEmptySet}
import cats.implicits._
import enumeratum._
import io.circe._
import io.circe.generic.JsonCodec
import swaggins.core.implicits._
import swaggins.openapi.model.components.SchemaName
import swaggins.openapi.model.shared.Reference.Able

import scala.collection.immutable

sealed trait Schema extends Product with Serializable

object Schema {
  implicit val decoder: Decoder[Schema] = {
    val decoders: SchemaType => Decoder[_ <: Schema] = {
      case SchemaType.Object => Decoder[ObjectSchema]
      case SchemaType.Array  => Decoder[ArraySchema]
      case SchemaType.Number => Decoder[NumberSchema]
      case SchemaType.String => Decoder[StringSchema]
    }

    val byType = for {
      tpe    <- Decoder[SchemaType].prepare(_.downField("type"))
      schema <- decoders(tpe).widen[Schema]
    } yield schema

    byType.or(Decoder[CompositeSchema].widen[Schema])
  }
}

case class CompositeSchema(schemas: NonEmptyList[Reference.Able[Schema]],
                           kind: CompositeSchemaKind,
                           discriminator: Option[Discriminator])
    extends Schema

object CompositeSchema {
  private object util {

    def notOne[T: Show](actual: List[T], allowed: Traversable[T]): String = {
      val actualString: String =
        if (actual.isEmpty) "none" else show"${actual.mkString(",")}"

      s"Must be exactly one of: ${allowed.mkString(", ")}, found $actualString"
    }

    def onlyOrAll[T](list: List[T]): Either[List[T], T] = list match {
      case h :: Nil => Right(h)
      case _        => Left(list)
    }
  }

  private object decoding {

    private val schemaKinds: Map[String, CompositeSchemaKind] =
      CompositeSchemaKind.namesToValuesMap

    def schemasDecoder(
      kind: CompositeSchemaKind): Decoder[NonEmptyList[Able[Schema]]] =
      Decoder[NonEmptyList[Able[Schema]]].prepare(_.downField(kind.entryName))

    def findKind(obj: JsonObject): Either[String, CompositeSchemaKind] = {
      import util._

      val presentKinds = obj.keys.flatMap(schemaKinds.get).toList

      onlyOrAll(presentKinds)
        .leftMap(_.map(_.entryName))
        .leftMap(notOne(_, schemaKinds.keySet))
    }
  }

  implicit val decoder: Decoder[CompositeSchema] = {
    import decoding._

    val discriminatorDecoder =
      Decoder.decodeOption[Discriminator].prepare(_.downField("discriminator"))

    for {
      kind                     <- Decoder.decodeJsonObject.emap(decoding.findKind)
      (schemas, discriminator) <- (schemasDecoder(kind), discriminatorDecoder).tupled
    } yield CompositeSchema(schemas, kind, discriminator)
  }
}

@JsonCodec(decodeOnly = true)
case class Discriminator(propertyName: Option[SchemaName],
                         mapping: Option[NonEmptyMap[String, SchemaName]])

/**
  * $synthetic
  * */
sealed abstract class CompositeSchemaKind(name: String) extends EnumEntry {
  override def entryName: String = super.entryName.lowerHead
}

object CompositeSchemaKind extends Enum[CompositeSchemaKind] {
  override def values: immutable.IndexedSeq[CompositeSchemaKind] = findValues

  case object OneOf extends CompositeSchemaKind("oneOf")
  case object AnyOf extends CompositeSchemaKind("anyOf")
  case object AllOf extends CompositeSchemaKind("allOf")
}

/**
  * $synthetic
  * */
@JsonCodec(decodeOnly = true)
case class ObjectSchema(
  required: Option[NonEmptySet[SchemaName]],
  properties: NonEmptyList[Property]
) extends Schema

object ObjectSchema {

  //re-usable decoder of NEL[(K, V)] (like NEM[K, V] but ordered like the input)
  def pairNelDecoder[K: KeyDecoder, V: Decoder, U](
    untuple: (K, V) => U): Decoder[NonEmptyList[U]] = {
    val toNel: JsonObject => Decoder.Result[NonEmptyList[(String, Json)]] = {
      _.toList.toNel.toRight(DecodingFailure("Must be non-empty", Nil))
    }

    val decodeProperty: (String, Json) => Decoder.Result[U] =
      (key, valueJson) =>
        Decoder[V].decodeJson(valueJson).flatMap { value =>
          KeyDecoder[K]
            .apply(key)
            .map(untuple(_, value))
            .toRight(DecodingFailure("Invalid key", Nil))
      }

    def decodeObject(obj: JsonObject): Decoder.Result[NonEmptyList[U]] = {
      obj.asRight.flatMap(toNel).flatMap(_.traverse(decodeProperty.tupled))
    }

    Decoder.decodeJsonObject.emapTry(decodeObject(_).toTry)
  }

  implicit val propNelDecoder: Decoder[NonEmptyList[Property]] = {
    pairNelDecoder(Property)
  }

}

/**
  * $synthetic
  * */
case class Property(name: SchemaName, schema: Reference.Able[Schema])

/**
  * $synthetic
  * */
@JsonCodec(decodeOnly = true)
case class ArraySchema(
  items: Reference.Able[Schema]
) extends Schema

trait PrimitiveSchema[Literal] extends Schema {
  def enum: Option[NonEmptySet[Literal]]
}

/**
  * $synthetic
  * */
@JsonCodec(decodeOnly = true)
case class NumberSchema(enum: Option[NonEmptySet[Double]])
    extends PrimitiveSchema[Double]

/**
  * $synthetic
  * */
@JsonCodec(decodeOnly = true)
case class StringSchema(enum: Option[NonEmptySet[String]])
    extends PrimitiveSchema[String]

/**
  * $synthetic
  * */
sealed trait SchemaType extends EnumEntry {
  override def entryName: String = super.entryName.toLowerCase
}

object SchemaType extends Enum[SchemaType] {

  override def values: immutable.IndexedSeq[SchemaType] = findValues

  case object Number extends SchemaType

  case object String extends SchemaType

  case object Object extends SchemaType

  case object Array extends SchemaType

  implicit val decoder: Decoder[SchemaType] = Decoder[String].emap { typeText =>
    withNameLowercaseOnlyOption(typeText).toRight(s"Unknown type: $typeText")
  }
}
