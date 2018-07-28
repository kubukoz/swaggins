package swaggins.generator
import swaggins.openapi.model.OpenAPI

trait Generator[F[_]] {
  //todo incomplete - needs resolved $ref-s
  def generate(spec: OpenAPI): fs2.Stream[F, GeneratedFile]
}
