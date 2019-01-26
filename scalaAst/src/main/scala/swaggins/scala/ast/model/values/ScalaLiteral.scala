package swaggins.scala.ast.model.values

import cats.implicits._
import cats.{Order, Show}
import scalaz.deriving
import swaggins.scala.ast.ref.TypeName

@deriving(Order)
sealed trait ScalaLiteral extends Product with Serializable {

  def asTypeName: TypeName = this match {
    case ScalaLiteral.String(value) => TypeName.parse(value)
    case ScalaLiteral.Double(value) => TypeName.raw(show"`$value`")
  }
}

object ScalaLiteral {
  final case class String(value: scala.Predef.String) extends ScalaLiteral

  final case class Double(value: scala.Double) extends ScalaLiteral

  //dumb constructor
  def string(value: scala.Predef.String): ScalaLiteral = String(value)

  implicit val show: Show[ScalaLiteral] = {
    case String(value) => '"' + value + '"'
    case Double(value) => value.toString + "d"
  }
}
