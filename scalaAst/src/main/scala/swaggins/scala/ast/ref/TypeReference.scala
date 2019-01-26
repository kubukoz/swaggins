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

  def listOf(elementType: TypeReference): TypeReference =
    AppliedType(scalaList, List(elementType), Nil)

  def optional(elementType: TypeReference): TypeReference =
    AppliedType(scalaOption, List(elementType), Nil)

  def byName(typeName: TypeName): TypeReference = OrdinaryType(typeName.value)

  val scalaList: TypeReference = OrdinaryType("List")
  val scalaOption: TypeReference = OrdinaryType("Option")
}

case class OrdinaryType(value: String) extends TypeReference {
  override def show: String = value.toCamelCase
}

@xderiving(Show)
case class Parameter(value: String) extends AnyVal

object Parameter {
  val fromLiteral: ScalaLiteral => Parameter = lit => Parameter(lit.show)
}

/**
  * An applied (higher-kinded) type, i.e. applied[params]
  * */
case class AppliedType(applied: TypeReference,
                       typeParams: List[TypeReference],
                       //todo do params belong here?
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
  //todo rename to "decode" or "fromSchema"
  def parse(value: String): TypeName = TypeName(value.toCamelCase)
  def raw(value: String): TypeName   = TypeName(value)
}
