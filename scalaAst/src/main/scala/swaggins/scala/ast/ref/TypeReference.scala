package swaggins.scala.ast.ref

import cats.Show
import cats.implicits._
import swaggins.core.implicits._

sealed trait TypeReference extends Product with Serializable {
  def show: String
}

object TypeReference {
  implicit val show: Show[TypeReference] = _.show
}

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

case class TypeName private (value: String) extends AnyVal

object TypeName {
  def parse(value: String): TypeName = TypeName(value.toCamelCase)
  def raw(value: String): TypeName = TypeName(value)

  implicit val show: Show[TypeName] = Show.show(_.value)
}
