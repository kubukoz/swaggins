package swaggins.config.model.sources

import cats.data.{NonEmptyList, NonEmptyMap}
import cats.implicits._
import io.circe.Decoder
import io.circe.generic.extras.semiauto._
import swaggins.config.model.shared.SourceIdentifier
import swaggins.core.implicits._

case class Sources(
  value: NonEmptyMap[SourceIdentifier, NonEmptyList[SourceUri]])
    extends AnyVal

object Sources {
  implicit val decoder: Decoder[Sources] = deriveUnwrappedDecoder
}

case class SourceUri(scheme: String, path: String)

object SourceUri {
  implicit val decoder: Decoder[SourceUri] = {
    Decoder[String].emap { string =>
      string.split(":").toList match {
        case scheme :: path :: Nil => SourceUri(scheme, path).asRight
        case _                     => s"Invalid source URI format: $string".asLeft
      }
    }
  }
}
