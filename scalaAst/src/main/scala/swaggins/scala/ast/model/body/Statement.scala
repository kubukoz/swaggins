package swaggins.scala.ast.model.body

import cats.Show
import cats.kernel.Order
import scalaz.deriving
import swaggins.scala.ast.model.ScalaModel

@deriving(Order)
private[model] sealed trait Statement extends Product with Serializable

object Statement {
  case class ModelStatement private (model: ScalaModel) extends Statement

  def model(underlying: ScalaModel): Statement = ModelStatement(underlying)

  implicit val show: Show[Statement] = {
    case ModelStatement(model) => model.show
  }
}

@deriving(Order)
final case class Body private (statements: List[Statement])

object Body {
  private[model] val empty: Body = Body(Nil)

  def models(models: List[ScalaModel]): Body = Body(models.map(Statement.model))
}
