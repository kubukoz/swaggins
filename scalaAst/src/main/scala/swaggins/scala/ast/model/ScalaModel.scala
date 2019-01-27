package swaggins.scala.ast.model

import cats.data.{NonEmptyList, NonEmptySet}
import cats.implicits._
import cats.{Functor, Order, Show}
import monocle._
import scalaz.deriving
import swaggins.core.implicits._
import swaggins.scala.ast.model.body._
import swaggins.scala.ast.model.klass.{ClassField, Constructor, FieldName}
import swaggins.scala.ast.model.modifiers.{Modifier, Modifiers}
import swaggins.scala.ast.model.values.ScalaLiteral
import swaggins.scala.ast.packages.Packages
import swaggins.scala.ast.ref._

@deriving(Order)
sealed trait ScalaModel extends Product with Serializable {
  def show: String
}

object ScalaModel {

  implicit val show: Show[ScalaModel] = _.show

  val extendsClause: Lens[ScalaModel, ExtendsClause] =
    Lens[ScalaModel, ExtendsClause] {
      case c: Klass             => c.extendsClause
      case t: Trait             => t.extendsClause
      case obj: SingletonObject => obj.extendsClause
    } { clause =>
      {
        case cc: Klass            => cc.copy(extendsClause = clause)
        case t: Trait             => t.copy(extendsClause = clause)
        case obj: SingletonObject => obj.copy(extendsClause = clause)
      }
    }

  def caseObject(name: TypeName,
                 extendsClause: ExtendsClause,
                 body: Body): ScalaModel =
    SingletonObject(Modifiers.of(Modifier.Case), name, extendsClause, body)

  def companionObject(name: TypeName, body: Body): ScalaModel = {
    SingletonObject(Modifiers.empty, name, ExtendsClause.empty, body)
  }

  def valueClass(name: TypeName, underlying: TypeReference): ScalaModel =
    finalCaseClass(
      name,
      NonEmptyList.one(
        ClassField(FieldName("value"), underlying)
      ),
      ExtendsClause.anyVal
    )

  def finalCaseClass(name: TypeName,
                     fields: NonEmptyList[ClassField],
                     extendsClause: ExtendsClause): ScalaModel =
    Klass(Modifiers.of(Modifier.Final, Modifier.Case),
          name,
          Constructor(fields),
          extendsClause)

  def enumeration[F[_]: Packages.Ask: Functor](
    name: TypeName,
    underlyingType: TypeReference,
    values: NonEmptySet[ScalaLiteral]
  ): F[ModelWithCompanion] = {

    /**
      * The base class for enum implementations
      * */
    val cls = Klass(
      Modifiers.of(Modifier.Sealed, Modifier.Abstract),
      name,
      Constructor.withFields(ClassField(FieldName("value"), underlyingType)),
      ExtendsClause.productWithSerializable
    )

    TypeReference.byName[F](name).map { tpe =>
      val inhabitantModels: NonEmptySet[ScalaModel] = values.map { str =>
        ScalaModel.caseObject(
          str.asTypeName,
          ExtendsClause.appliedType(tpe, Parameter.fromLiteral(str) :: Nil),
          Body.empty)
      }

      /**
        * The object with enum implementations
        * */
      val obj = SingletonObject(Modifiers.empty,
                                name,
                                ExtendsClause.empty,
                                Body.models(inhabitantModels.toList))

      ModelWithCompanion.both(cls, obj)
    }
  }

  def sealedTraitHierarchy(
    name: TypeName,
    inhabitants: NonEmptyList[ScalaModel]
  ): ModelWithCompanion = {
    val rootNode = sealedTrait(name, ExtendsClause.productWithSerializable)

    val leafNodes = SingletonObject(Modifiers.empty,
                                    name,
                                    ExtendsClause.empty,
                                    Body.models(inhabitants.toList))
    ModelWithCompanion.both(rootNode, leafNodes)
  }

  def sealedTrait(name: TypeName, extendsClause: ExtendsClause): ScalaModel =
    Trait(Modifiers.of(Modifier.Sealed), name, extendsClause)
}

@deriving(Order)
final case class Trait(mods: Modifiers,
                       name: TypeName,
                       extendsClause: ExtendsClause)
    extends ScalaModel {
  override def show: String = show"""${mods}trait $name$extendsClause"""
}

final case class SingletonObject(mods: Modifiers,
                                 name: TypeName,
                                 extendsClause: ExtendsClause,
                                 body: Body)
    extends ScalaModel {
  private val bodyBlock = body.statements match {
    case Nil => ""
    case _ =>
      body.statements.map(_.show.indented(2)).mkString_(" {\n", "\n", "\n}")
  }

  override def show: String =
    show"""${mods}object $name$extendsClause$bodyBlock"""
}

final case class Klass(
  mods: Modifiers,
  name: TypeName,
  constructor: Constructor,
  extendsClause: ExtendsClause,
) extends ScalaModel {

  override def show: String =
    show"""${mods}class $name$constructor$extendsClause"""
}

final case class ExtendsClause(refs: List[TypeReference]) extends AnyVal

object ExtendsClause {
  implicit val show: Show[ExtendsClause] = _.refs match {
    case Nil  => ""
    case refs => refs.mkString_(" extends ", " with ", "")
  }

  val empty: ExtendsClause = ExtendsClause(Nil)

  val productWithSerializable = ExtendsClause(
    List(TypeReference.product, TypeReference.serializable)
  )

  val anyVal = ExtendsClause(List(TypeReference.anyVal))

  def single(ref: TypeReference): ExtendsClause = apply(List(ref))

  def appliedType(ref: TypeReference, params: List[Parameter]): ExtendsClause =
    single(AppliedType(ref, Nil, params))
}
