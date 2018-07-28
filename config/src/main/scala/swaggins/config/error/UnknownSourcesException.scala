package swaggins.config.error
import cats.data.NonEmptyList
import swaggins.config.model.shared.SourceIdentifier

case class UnknownSourcesException(sources: NonEmptyList[SourceIdentifier])
    extends ConfigValidationException {

  override val getMessage: String = {
    val sourcesText = sources.map(_.value).toList.mkString(",")

    s"""Unknown sources: $sourcesText. Did you define them in the "sources" property?"""
  }
}

trait ConfigValidationException extends Exception with Product with Serializable
