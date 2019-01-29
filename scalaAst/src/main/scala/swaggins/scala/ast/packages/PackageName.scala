package swaggins.scala.ast.packages

import cats.Show
import cats.data.Chain
import cats.implicits._
import cats.kernel.Order
import cats.mtl.{ApplicativeAsk, ApplicativeLocal}
import scalaz.xderiving
import swaggins.core.implicits._
import swaggins.scala.ast.ref.Typ

@xderiving(Show, Order)
final case class PackageName(value: String) extends AnyVal

@xderiving(Order)
//todo rename to type qualifiers/paths?
//TypePath(TypeQualifier*)
final case class Packages(value: Chain[PackageName]) extends AnyVal {
  def append(pkg: PackageName): Packages = copy(value append pkg)
  def isEmpty: Boolean = value.isEmpty
}

object Packages {
  def fromTyp(typ: Typ): Packages = typ.pkg.append(PackageName(typ.name.value))

  implicit val show: Show[Packages] = _.value.mkString_("", ".", "")

  type Ask[F[_]] = ApplicativeAsk[F, Packages]
  def Ask[F[_]](implicit F: Ask[F]): Ask[F] = F

  type Local[F[_]] = ApplicativeLocal[F, Packages]
  def Local[F[_]](implicit F: Local[F]): Local[F] = F

  val empty: Packages = Packages(Chain.empty)
  def of(packages: PackageName*): Packages = Packages(Chain.fromSeq(packages))

  val rootScala: Packages       = Packages.of(PackageName("_root_"), PackageName("scala"))
  val rootScalaPredef: Packages = rootScala.append(PackageName("Predef"))
}
