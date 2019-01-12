package swaggins.config.model.code
import cats.Order
import cats.data.NonEmptyMap
import io.circe.{Decoder, KeyDecoder}
import io.circe.generic.extras.semiauto._
import cats.implicits._
import io.circe.generic.JsonCodec
import swaggins.config.model.shared.SourceIdentifier

case class Code(value: NonEmptyMap[SourceIdentifier, SourceSpecs])
    extends AnyVal

object Code {
  implicit val decoder: Decoder[Code] = deriveUnwrappedDecoder
}

case class SourceSpecs(value: NonEmptyMap[SpecIdentifier, SpecGenerators])
    extends AnyVal

object SourceSpecs {
  implicit val decoder: Decoder[SourceSpecs] = deriveUnwrappedDecoder
}

case class SpecIdentifier(name: String, version: String)

object SpecIdentifier {
  implicit val decoder: KeyDecoder[SpecIdentifier] = KeyDecoder.instance {
    _.split(":").toList match {
      case name :: version :: Nil => SpecIdentifier(name, version).some
      case _                      => none
    }
  }

  implicit val order: Order[SpecIdentifier] =
    Order.by(ident => (ident.name, ident.version))
}

case class SpecGenerators(value: NonEmptyMap[GeneratorKey, GeneratorConfig])
    extends AnyVal

object SpecGenerators {
  implicit val decoder: Decoder[SpecGenerators] = deriveUnwrappedDecoder
}

case class GeneratorKey(value: String) extends AnyVal

object GeneratorKey {
  implicit val decoder: KeyDecoder[GeneratorKey] =
    KeyDecoder.instance(apply(_).some)

  implicit val order: Order[GeneratorKey] = Order.by(_.value)
}

@JsonCodec(decodeOnly = true)
case class GeneratorConfig(path: String)
