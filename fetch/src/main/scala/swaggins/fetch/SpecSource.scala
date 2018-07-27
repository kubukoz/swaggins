package swaggins.fetch
import swaggins.openapi.model.OpenAPI

//todo probably needs spec sources from config too
trait SpecSource[F[_]] {
  def directDependencies(spec: OpenAPI): fs2.Stream[F, OpenAPI]
}
