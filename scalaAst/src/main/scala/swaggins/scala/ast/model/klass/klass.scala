package swaggins.scala.ast.model.klass

import cats.{Order, Show}
import cats.data.NonEmptyList
import cats.implicits._
import scalaz.deriving
import swaggins.scala.ast.ref.TypeReference
import swaggins.core.implicits._

@deriving(Order)
private[model] final case class Constructor private (fields: NonEmptyList[ClassField])

private[model] object Constructor {

  def withFields(field1: ClassField, fields: ClassField*): Constructor =
    Constructor(NonEmptyList(field1, fields.toList))

  implicit val show: Show[Constructor] = cons =>
    cons.fields.mkString_("(", ", ", ")")
}

@deriving(Order)
final case class ClassField(
  name: FieldName,
  tpe: TypeReference
)

object ClassField {
  implicit val show: Show[ClassField] = field =>
    show"""${field.name}: ${field.tpe}"""
}

@deriving(Order)
final case class FieldName(value: String) extends AnyVal

object FieldName {
  implicit val show: Show[FieldName] = Show.show(_.value.toCamelCaseLower)
}
