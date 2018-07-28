package swaggins.config.model.shared
import io.circe.KeyDecoder
import cats.implicits._

case class SourceIdentifier(value: String) extends AnyVal

object SourceIdentifier {
  implicit val ordering: Ordering[SourceIdentifier] = Ordering.by(_.value)

  implicit val decoder: KeyDecoder[SourceIdentifier] =
    KeyDecoder.instance(apply(_).some)
}
