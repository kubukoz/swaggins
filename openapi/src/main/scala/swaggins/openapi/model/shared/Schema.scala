package swaggins.openapi.model.shared
import cats.data.{NonEmptyList, NonEmptyMap}
import cats.implicits._
import enumeratum._
import io.circe.Decoder
import io.circe.generic.JsonCodec
import swaggins.core.implicits._
import swaggins.openapi.model.components.SchemaName

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

    for {
      tpe    <- Decoder[SchemaType].prepare(_.downField("type"))
      schema <- decoders(tpe).widen[Schema]
    } yield schema
  }
}

/**
  * $synthetic
  * */
@JsonCodec(decodeOnly = true)
case class ObjectSchema(
  required: Option[NonEmptyList[SchemaName]],
  properties: Option[NonEmptyMap[SchemaName, Reference.Able[Schema]]]
) extends Schema

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
@JsonCodec(decodeOnly = true)
case class NumberSchema() extends Schema

/**
  * $synthetic
  * */
@JsonCodec(decodeOnly = true)
case class StringSchema() extends Schema

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
