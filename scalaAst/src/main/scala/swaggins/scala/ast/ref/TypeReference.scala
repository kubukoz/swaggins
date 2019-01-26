package swaggins.scala.ast.ref

import cats.{Order, Show}
import cats.implicits._
import scalaz.{deriving, xderiving}
import swaggins.core.implicits._
import swaggins.scala.ast.model.ScalaLiteral

@deriving(Order)
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

@xderiving(Show)
case class Parameter(value: String) extends AnyVal

object Parameter {
  val fromLiteral: ScalaLiteral => Parameter = lit => Parameter(lit.show)
}

case class AppliedType(applied: TypeName,
                       typeParams: List[TypeReference],
                       params: List[Parameter])
    extends TypeReference {

  private val typeParamListString =
    if (typeParams.nonEmpty) typeParams.mkString_("[", ", ", "]") else ""
  private val paramListString =
    if (params.nonEmpty) params.mkString_("(", ", ", ")") else ""

  override def show: String = show"$applied$typeParamListString$paramListString"
}

object Primitive {
  object Double extends OrdinaryType("Double")
  object String extends OrdinaryType("String")
}

@deriving(Order)
@xderiving(Show)
case class TypeName private (value: String) extends AnyVal

object TypeName {
  def parse(value: String): TypeName = TypeName(value.toCamelCase)
  def raw(value: String): TypeName   = TypeName(value)
}
