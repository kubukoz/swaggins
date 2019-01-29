package swaggins.scala.ast.ref

import cats.{Order, Show}
import cats.implicits._
import scalaz.{deriving, xderiving}
import swaggins.core.implicits._
import swaggins.scala.ast.model.values.ScalaLiteral
import swaggins.scala.ast.packages.Packages

@xderiving(Order, Show)
case class TypeName2(value: String) extends AnyVal

@deriving(Order)
case class Typ(name: TypeName2, pkg: Packages)

object Typ {

  def under(name: TypeName2, parent: Typ): Typ =
    Typ(name, Packages.fromTyp(parent))

  //todo think about these two - types that can be applied can't be used as names (or can they?)
  def listOf(elementType: TypeReference): TypeReference =
    AppliedType(scalaList, List(elementType), Nil)

  def optional(elementType: TypeReference): TypeReference =
    AppliedType(scalaOption, List(elementType), Nil)

  val scalaList: Typ    = Typ(TypeName2("List"), Packages.rootScala)
  val scalaOption: Typ  = Typ(TypeName2("Option"), Packages.rootScala)
  val product: Typ      = Typ(TypeName2("Product"), Packages.rootScala)
  val serializable: Typ = Typ(TypeName2("Serializable"), Packages.rootScala)
  val anyVal: Typ       = Typ(TypeName2("AnyVal"), Packages.rootScala)
  val double: Typ       = Typ(TypeName2("Double"), Packages.rootScala)
  val string: Typ       = Typ(TypeName2("String"), Packages.rootScalaPredef)
}

@xderiving(Order, Show)
case class TypeReference(value: String)

object TypeReference {
  def fromType(typ: Typ): TypeReference = {
    val prefix = if (typ.pkg.isEmpty) "" else typ.pkg.show + "."

    TypeReference(show"$prefix${typ.name}")
  }
}

final case class OrdinaryType(value: String) extends TypeReference {
  override val show: String = value
}

@xderiving(Show)
final case class Parameter(value: String) extends AnyVal

object Parameter {
  val fromLiteral: ScalaLiteral => Parameter = lit => Parameter(lit.show)
}

/**
  * An applied (higher-kinded) type, i.e. applied[params]
  * */
final case class AppliedType(applied: TypeReference,
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

case class QualifiedReference(pkg: Packages, ref: TypeReference)
    extends TypeReference {
  override def show: String = if (pkg.isEmpty) ref.show else show"$pkg.$ref"
}

@xderiving(Show, Order)
@deprecated
final case class TypeName private (value: String) extends AnyVal

object TypeName {
  //todo rename to "decode" or "fromSchema"
  def parse(value: String): TypeName = TypeName(value.toCamelCase)
  def raw(value: String): TypeName   = TypeName(value)
}

object PrimitiveNames {
  val double: TypeName = TypeName.raw("Double")
  val string: TypeName = TypeName.raw("String")
}
