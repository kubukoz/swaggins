package swaggins.generator
import cats.effect.Sync
import cats.implicits._
import fs2.Stream
import swaggins.generator.convert.Converters
import swaggins.openapi.model.OpenAPI
import swaggins.openapi.model.components.SchemaName
import swaggins.openapi.model.shared.Reference.Able
import swaggins.openapi.model.shared._

trait Generator[F[_]] {
  def generate(spec: OpenAPI): Stream[F, GeneratedFile]
}

class ScalaCaseClassGenerator[F[_]: Sync] extends Generator[F] {

  def generate(spec: OpenAPI): Stream[F, GeneratedFile] = {
    val componentList: List[(SchemaName, Able[Schema])] =
      spec.components.schemas.toSortedMap.toList

    val componentStrings: Stream[F, String] =
      Stream
        .emits(componentList)
        .map((Converters.convertSchemaOrRef _).tupled)
        .flatMap(elems => Stream.emits(elems.toList))
        .map(_.show)

    Stream.emit(componentStrings).evalMap { fileStream =>
      fileStream.compile.toList.map { lines =>
        GeneratedFile("models.scala",
                      lines.mkString_("package models\n\n", "\n", "\n"))
      }
    }
  }

}
