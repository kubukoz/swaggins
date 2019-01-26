package swaggins.openapi.model.shared

import cats.data.NonEmptyList
import io.circe.Decoder
import swaggins.openapi.model.components.SchemaName
import cats.implicits._
import scalaz.deriving

@deriving(Decoder)
case class Reference(`$ref`: ReferenceRef)

object Reference {
  //a reference-able type.
  type Able[T] = Either[Reference, T]

  implicit def decoder[T: Decoder]: Decoder[Able[T]] =
    Decoder[Reference].either(Decoder[T])

}

/**
  * $synthetic
  * */
sealed trait ReferenceRef extends Product with Serializable

object ReferenceRef {
  case class ComponentRef(name: SchemaName) extends ReferenceRef

  object ComponentRef {
    implicit val decoder: Decoder[ComponentRef] = {
      val pattern = """\#\/components\/schemas\/(.+)""".r

      Decoder[String].emap {
        case pattern(name) => ComponentRef(SchemaName(name)).asRight
        case str           => s"Invalid component reference: $str".asLeft
      }
    }
  }

  implicit val decoder: Decoder[ReferenceRef] =
    NonEmptyList
      .of(
        Decoder[ComponentRef].widen[ReferenceRef]
      )
      .reduceK

}
