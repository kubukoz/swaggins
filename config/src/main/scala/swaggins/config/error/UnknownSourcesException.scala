package swaggins.config.error
import cats.data.NonEmptySet
import cats.implicits._
import swaggins.config.model.shared.SourceIdentifier

case class UnknownSourcesException(sources: NonEmptySet[SourceIdentifier])
    extends ConfigValidationException {

  override val getMessage: String = {
    val sourcesText = sources.map(_.value).toList.mkString(",")

    s"""Unknown sources: $sourcesText. Did you define them in the "sources" property?"""
  }
}

trait ConfigValidationException extends Exception with Product with Serializable
