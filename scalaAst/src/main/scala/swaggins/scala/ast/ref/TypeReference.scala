package swaggins.scala.ast.ref

import cats.{Order, Show}
import cats.implicits._
import scalaz.{deriving, xderiving}
import swaggins.core.implicits._
import swaggins.scala.ast.model.values.ScalaLiteral
import swaggins.scala.ast.packages.{PackageName, Packages}

import scala.annotation.tailrec

@deriving(Order)
sealed trait TypeReference extends Product with Serializable {
  def show: String
}

object TypeReference {

  def byName[F[_]: Packages.Ask](base: TypeName): F[TypeReference] = {
    Packages
      .Ask[F]
      .reader(pkg => QualifiedReference(pkg, OrdinaryType(base.show)))
  }

  implicit val show: Show[TypeReference] = _.show

  def listOf(elementType: TypeReference): TypeReference =
    AppliedType(scalaList, List(elementType), Nil)

  def optional(elementType: TypeReference): TypeReference =
    AppliedType(scalaOption, List(elementType), Nil)

  val scalaList: TypeReference   = OrdinaryType.apply("_root_.scala.List")
  val scalaOption: TypeReference = OrdinaryType.apply("_root_.scala.Option")
  val product: TypeReference     = OrdinaryType.apply("_root_.scala.Product")

  val serializable: TypeReference =
    OrdinaryType.apply("_root_.scala.Serializable")
  val anyVal: TypeReference = OrdinaryType.apply("_root_.scala.AnyVal")
  val double: TypeReference = OrdinaryType.apply("_root_.scala.Double")
  val string: TypeReference = OrdinaryType.apply("_root_.scala.Predef.String")
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

@deriving(Order)
sealed trait TypeName extends Product with Serializable {

  final def toPackages: Packages = {
    @tailrec
    def go(elem: TypeName, mem: Packages): Packages = elem match {
      case Parsed(value) => Packages.one(PackageName(value)).concat(mem)
      case Anonymous(number, scope) =>
        go(scope, mem.append(PackageName(show"Anonymous$$$number")))
    }

    go(this, Packages.empty)
  }
}

final case class Parsed(value: String)                   extends TypeName
final case class Anonymous(number: Int, scope: TypeName) extends TypeName

object TypeName {
  implicit val show: Show[TypeName] = {
    case Parsed(value)        => value
    case Anonymous(number, _) => show"Anonymous$$$number"
  }
  //todo rename to "decode" or "fromSchema"
  def parse(value: String): TypeName = Parsed(value.toCamelCase)
  def raw(value: String): TypeName   = Parsed(value)

  def anonymous(number: Int, scope: TypeName): TypeName =
    Anonymous(number, scope)
}

object PrimitiveNames {
  val double: TypeName = TypeName.raw("Double")
  val string: TypeName = TypeName.raw("String")
}
