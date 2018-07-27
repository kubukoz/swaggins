package swaggins.generator
import swaggins.openapi.model.OpenAPI

trait Generator[F[_]] {
  def generate(spec: OpenAPI): fs2.Stream[F, GeneratedFile]
}
