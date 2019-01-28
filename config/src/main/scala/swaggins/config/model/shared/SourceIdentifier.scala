package swaggins.config.model.shared

import cats.Order
import io.circe.KeyDecoder
import swaggins.core.implicits._
import scalaz.xderiving
import cats.implicits._

@xderiving(Order, KeyDecoder)
final case class SourceIdentifier(value: String) extends AnyVal
