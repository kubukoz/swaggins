package swaggins.scala.ast

import cats.Show
import cats.data.NonEmptyList
import swaggins.core.implicits._
import cats.implicits._
import enumeratum.{Enum, EnumEntry}

import scala.collection.immutable

sealed trait TypeReference extends Product with Serializable {
  def show: String
}

object TypeReference {
  implicit val show: Show[TypeReference] = Show.show(_.show)

  case class OrdinaryType(value: String) extends TypeReference {
    override def show: String = value.toCamelCase
  }

  case class ListType(itemType: TypeReference) extends TypeReference {
    override def show: String = show"""List[$itemType]"""
  }

  object Primitive {
    object Double extends OrdinaryType("Double")
    object String extends OrdinaryType("String")
  }
}

case class TypeName(value: String) extends AnyVal

object TypeName {
  implicit val show: Show[TypeName] = Show.show(_.value.toCamelCase)
}

case class FieldName(value: String) extends AnyVal

object FieldName {
  implicit val show: Show[FieldName] = Show.show(_.value.toCamelCaseLower)
}

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
