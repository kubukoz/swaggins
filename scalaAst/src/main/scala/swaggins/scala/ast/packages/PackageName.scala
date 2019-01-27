package swaggins.scala.ast.packages

import cats.Show
import cats.data.Chain
import cats.implicits._
import cats.kernel.Order
import cats.mtl.{ApplicativeAsk, ApplicativeLocal}
import scalaz.xderiving
import swaggins.core.implicits._

@xderiving(Show, Order)
final case class PackageName(value: String) extends AnyVal

@xderiving(Order)
final case class Packages(value: Chain[PackageName]) extends AnyVal {
  def append(pkg: PackageName): Packages = copy(value append pkg)
  def concat(pkgs: Packages): Packages   = copy(value concat pkgs.value)
  def isEmpty: Boolean                   = value.isEmpty
}

object Packages {
  implicit val show: Show[Packages] = _.value.mkString_("", ".", "")

  type Ask[F[_]] = ApplicativeAsk[F, Packages]
  def Ask[F[_]](implicit F: Ask[F]): Ask[F] = F

  type Local[F[_]] = ApplicativeLocal[F, Packages]
  def Local[F[_]](implicit F: Local[F]): Local[F] = F

  val empty: Packages                  = Packages(Chain.empty)
  def one(name: PackageName): Packages = Packages(Chain.one(name))
}
