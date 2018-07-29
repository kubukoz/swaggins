package swaggins.openapi.model.shared

import cats.Show
import cats.data.NonEmptyList
import cats.implicits._
import enumeratum._
import io.circe._
import io.circe.generic.JsonCodec
import swaggins.openapi.model.components.SchemaName
import swaggins.openapi.model.shared.Reference.Able
import swaggins.core.implicits._
import scala.collection.immutable

sealed trait Schema extends Product with Serializable

object Schema {
  implicit val decoder: Decoder[Schema] = {
    val decoders: SchemaType => Decoder[_ <: Schema] = {
      case SchemaType.Object => Decoder[ObjectSchema]
      case SchemaType.Array  => Decoder[ArraySchema]
      case SchemaType.Number => Decoder[NumberSchema.type]
      case SchemaType.String => Decoder[StringSchema.type]
    }

    val byType = for {
      tpe    <- Decoder[SchemaType].prepare(_.downField("type"))
      schema <- decoders(tpe).widen[Schema]
    } yield schema

    byType.or(Decoder[CompositeSchema].widen[Schema])
  }
}

case class CompositeSchema(schemas: NonEmptyList[Reference.Able[Schema]],
                           kind: CompositeSchemaKind)
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

    val schemasDecoder: Decoder[NonEmptyList[Able[Schema]]] =
      Decoder[NonEmptyList[Able[Schema]]]

    private val schemaKinds: Map[String, CompositeSchemaKind] =
      CompositeSchemaKind.namesToValuesMap

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

    for {
      kind    <- Decoder.decodeJsonObject.emap(decoding.findKind)
      schemas <- schemasDecoder.prepare(_.downField(kind.entryName))
    } yield CompositeSchema(schemas, kind)
  }
}

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
  required: Option[NonEmptyList[SchemaName]],
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

/**
  * $synthetic
  * */
case object NumberSchema extends Schema {
  implicit val decoder: Decoder[NumberSchema.type] = Decoder.const(NumberSchema)
}

/**
  * $synthetic
  * */
case object StringSchema extends Schema {
  implicit val decoder: Decoder[StringSchema.type] = Decoder.const(StringSchema)
}

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
