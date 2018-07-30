package swaggins.openapi.model.shared

import io.circe.Decoder
import io.circe.generic.JsonCodec
import io.circe.generic.extras.semiauto._

@JsonCodec(decodeOnly = true)
case class Reference(`$ref`: ReferenceString)

object Reference {
  //a reference-able type.
  type Able[T] = Either[Reference, T]

  implicit def decoder[T: Decoder]: Decoder[Able[T]] =
    Decoder[Reference].either(Decoder[T])
}

/**
  * $synthetic
  * */
case class ReferenceString(value: String) extends AnyVal

object ReferenceString {
  implicit val decoder: Decoder[ReferenceString] = deriveUnwrappedDecoder
}
