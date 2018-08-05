package swaggins.scala.ast.model

import cats.Show
import cats.data.NonEmptyList
import cats.implicits._
import swaggins.core.implicits._
import swaggins.scala.ast.ref.{TypeName, TypeReference}

sealed trait ScalaModel extends Product with Serializable {
  def show: String
}

case class CaseClass(name: TypeName, fields: NonEmptyList[CaseClassField])
    extends ScalaModel {
  override def show: String =
    show"""case class $name(${fields.mkString_("", ", ", "")})"""
}

case class ValueClass(name: TypeName, tpe: TypeReference) extends ScalaModel {
  private val fieldName: FieldName = FieldName("value")
  private val field: CaseClassField =
    CaseClassField(required = true, fieldName, tpe)

  override def show: String = show"""case class $name($field) extends AnyVal"""
}

case class CaseClassField(required: Boolean,
                          name: FieldName,
                          tpe: TypeReference) {

  def show: String = {
    val fieldTypeString = {
      if (required) tpe.show else show"Option[$tpe]"
    }

    show"""$name: $fieldTypeString"""
  }
}

object CaseClassField {
  implicit val show: Show[CaseClassField] = Show.show(_.show)
}

case class FieldName(value: String) extends AnyVal

object FieldName {
  implicit val show: Show[FieldName] = Show.show(_.value.toCamelCaseLower)
}
