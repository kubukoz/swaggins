package swaggins.config.model.shared

import cats.Order
import io.circe.KeyDecoder
import cats.implicits._

final case class SourceIdentifier(value: String) extends AnyVal

object SourceIdentifier {
  implicit val order: Order[SourceIdentifier] = Order.by(_.value)

  implicit val decoder: KeyDecoder[SourceIdentifier] =
    KeyDecoder.instance(apply(_).some)
}
