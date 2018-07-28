package swaggins.config.model.sources

import cats.implicits._
import io.circe.Decoder
import io.circe.generic.extras.semiauto._
import swaggins.config.model.shared.SourceIdentifier

import scala.collection.immutable.SortedMap

case class Sources(value: SortedMap[SourceIdentifier, List[SourceUri]]) extends AnyVal

object Sources {
  {
    Decoder[SortedMap[String, String]]
  }
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
