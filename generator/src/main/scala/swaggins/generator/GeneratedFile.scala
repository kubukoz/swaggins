package swaggins.generator
import cats.Eq

final case class GeneratedFile(name: String, content: String)

object GeneratedFile {
  implicit val eq: Eq[GeneratedFile] = Eq.fromUniversalEquals
}
