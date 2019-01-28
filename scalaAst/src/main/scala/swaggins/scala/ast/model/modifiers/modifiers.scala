package swaggins.scala.ast.model.modifiers

import cats.{Order, Show}
import scalaz.{deriving, xderiving}
import cats.implicits._
import swaggins.core.implicits._

@deriving(Order)
sealed trait Modifier extends Product with Serializable

object Modifier {
  case object Sealed   extends Modifier
  case object Abstract extends Modifier
  case object Final    extends Modifier
  case object Case     extends Modifier

  implicit val show: Show[Modifier] = {
    case Case     => "case"
    case Final    => "final"
    case Sealed   => "sealed"
    case Abstract => "abstract"
  }
}

@xderiving(Order)
final case class Modifiers(value: List[Modifier]) extends AnyVal

object Modifiers {
  val empty: Modifiers               = Modifiers(Nil)
  def of(mods: Modifier*): Modifiers = Modifiers(mods.toList)

  implicit val show: Show[Modifiers] = {
    case `empty`         => ""
    case Modifiers(mods) => mods.mkString_("", " ", " ")
  }
}
