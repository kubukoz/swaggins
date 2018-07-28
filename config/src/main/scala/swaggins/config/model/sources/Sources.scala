package swaggins.config.model.sources

import cats.data.{NonEmptyList, NonEmptyMap}
import enumeratum._
import io.circe.Decoder
import io.circe.generic.extras.semiauto._
import swaggins.config.model.shared.SourceIdentifier
import swaggins.core.implicits._

import scala.collection.immutable

case class Sources(
  value: NonEmptyMap[SourceIdentifier, NonEmptyList[SourceUri]])
    extends AnyVal

object Sources {
  implicit val decoder: Decoder[Sources] = deriveUnwrappedDecoder
}

case class SourceUri(scheme: SourceScheme, path: String)

sealed abstract class SourceScheme(override val entryName: String)
    extends EnumEntry
    with Product
    with Serializable

object SourceScheme extends Enum[SourceScheme] {

  override def values: immutable.IndexedSeq[SourceScheme] = findValues

  case object Github     extends SourceScheme("gh")
  case object Filesystem extends SourceScheme("fs")
}

object SourceUri {
  implicit val decoder: Decoder[SourceUri] = {
    def splitByColon(string: String): Either[String, (String, String)] = {
      val tupleOpt = string.split(":").toList match {
        case scheme :: path :: Nil => Some((scheme, path))
        case _                     => None
      }

      tupleOpt.toRight(s"Invalid source URI format: $string")
    }

    def parseScheme(schemeText: String): Either[String, SourceScheme] =
      SourceScheme
        .withNameOption(schemeText)
        .toRight(s"Unknown source scheme: $schemeText")

    Decoder[String].emap { string =>
      for {
        (schemeText, path) <- splitByColon(string)
        scheme             <- parseScheme(schemeText)

      } yield SourceUri(scheme, path)
    }
  }
}
