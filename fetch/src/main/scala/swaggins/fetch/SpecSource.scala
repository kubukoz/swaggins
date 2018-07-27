package swaggins.fetch
import swaggins.openapi.model.OpenAPI

trait SpecSource[F[_]] {
  def directDependencies(spec: OpenAPI): fs2.Stream[F, OpenAPI]
}
