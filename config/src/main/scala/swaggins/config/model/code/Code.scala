package swaggins.config.model.code
import cats.Order
import cats.data.NonEmptyMap
import io.circe.{Decoder, KeyDecoder}
import cats.implicits._
import scalaz.{deriving, xderiving}
import swaggins.config.model.shared.SourceIdentifier
import swaggins.core.implicits._

@xderiving(Decoder)
final case class Code(value: NonEmptyMap[SourceIdentifier, SourceSpecs])
    extends AnyVal

object Code {
  //empty object for scalaz-deriving
}

@xderiving(Decoder)
final case class SourceSpecs(value: NonEmptyMap[SpecIdentifier, SpecGenerators])
    extends AnyVal

object SourceSpecs {
  //empty object for scalaz-deriving
}

@deriving(Order)
final case class SpecIdentifier(name: String, version: String)

object SpecIdentifier {
  implicit val decoder: KeyDecoder[SpecIdentifier] = KeyDecoder.instance {
    _.split(":").toList match {
      case name :: version :: Nil => SpecIdentifier(name, version).some
      case _                      => none
    }
  }
}

@xderiving(Decoder)
final case class SpecGenerators(value: NonEmptyMap[GeneratorKey, GeneratorConfig])
    extends AnyVal

object SpecGenerators {
  //empty object for scalaz-deriving
}

@xderiving(Order, KeyDecoder)
final case class GeneratorKey(value: String) extends AnyVal

@deriving(Decoder)
final case class GeneratorConfig(path: String)
