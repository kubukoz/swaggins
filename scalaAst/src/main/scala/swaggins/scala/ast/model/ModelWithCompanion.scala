package swaggins.scala.ast.model
import cats.Order
import cats.data.NonEmptyList
import monocle.Lens
import monocle.macros.Lenses
import scalaz.deriving

@deriving(Order)
@Lenses
final case class ModelWithCompanion(klass: ScalaModel,
                              companion: Option[ScalaModel]) {
  def asNel: NonEmptyList[ScalaModel] = NonEmptyList(klass, companion.toList)
}

object ModelWithCompanion {

  def justClass(klass: ScalaModel): ModelWithCompanion =
    ModelWithCompanion(klass, None)

  def both(klass: ScalaModel, companion: ScalaModel): ModelWithCompanion =
    ModelWithCompanion(klass, Some(companion))

  val klassExtendsClause: Lens[ModelWithCompanion, ExtendsClause] =
    klass.composeLens(ScalaModel.extendsClause)
}
