package swaggins.scala.ast.model

import cats.data.{NonEmptyList, NonEmptyMap, NonEmptySet}
import cats.implicits._
import cats.{Order, Show}
import swaggins.core.implicits._
import swaggins.scala.ast.ref._
import monocle._
import scalaz.deriving

@deriving(Order)
sealed trait ScalaModel extends Product with Serializable {
  def show: String
}

object ScalaModel {
  implicit val show: Show[ScalaModel] = _.show

  val extendsClause: Lens[ScalaModel, ExtendsClause] =
    Lens[ScalaModel, ExtendsClause] {
      case CaseClass(_, _, clause, _)            => clause
      case SealedTraitHierarchy(_, _, _, clause) => clause
      case Enumerated(_, _, _, clause)           => clause
    } { clause =>
      {
        case cc: CaseClass             => cc.copy(extendsClause = clause)
        case adt: SealedTraitHierarchy => adt.copy(extendsClause = clause)
        case enum: Enumerated          => enum.copy(extendsClause = clause)
      }
    }
}

@deriving(Order)
final case class CompanionObject(name: TypeName,
                                 models: NonEmptyList[ScalaModel])

object CompanionObject {
  implicit val show: Show[CompanionObject] = {
    case CompanionObject(name, models) =>
      show"""object $name {
            |${models.mkString_("", "\n", "").indented(2)}
            |}""".stripMargin
  }
}

object ValueClass {

  def apply(name: TypeName, tpe: TypeReference): CaseClass =
    CaseClass(
      name,
      NonEmptyList.one(
        CaseClassField(required = true, FieldName("value"), tpe)
      ),
      ExtendsClause.anyVal
    )
}

@deriving(Order)
final case class CaseClassField(
  required: Boolean,
  name: FieldName,
  tpe: TypeReference
) {

  def show: String = {
    val fieldTypeString = {
      if (required) tpe.show else show"Option[$tpe]"
    }

    show"""$name: $fieldTypeString"""
  }
}

object CaseClassField {
  implicit val show: Show[CaseClassField] = Show.show(_.show)
}

@deriving(Order)
final case class FieldName(value: String) extends AnyVal

object FieldName {
  implicit val show: Show[FieldName] = Show.show(_.value.toCamelCaseLower)
}

final case class ExtendsClause(refs: List[TypeReference]) extends AnyVal

object ExtendsClause {
  implicit val show: Show[ExtendsClause] = _.refs match {
    case Nil  => ""
    case refs => refs.mkString_(" extends ", " with ", "")
  }

  val empty: ExtendsClause = ExtendsClause(Nil)

  val productWithSerializable = ExtendsClause(
    List(OrdinaryType("Product"), OrdinaryType("Serializable"))
  )

  val anyVal = ExtendsClause(List(OrdinaryType("AnyVal")))

  def single(ref: TypeReference): ExtendsClause = apply(List(ref))

  def appliedType(name: TypeName, params: List[Parameter]): ExtendsClause =
    single(AppliedType(name, Nil, params))
}

@deriving(Order)
final case class Discriminator(
  propertyName: Option[FieldName],
  mapping: Option[NonEmptyMap[String, OrdinaryType]]
)

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

final case class CaseObject(name: TypeName, extendsClause: ExtendsClause)
    extends ScalaModel {
  override def show: String = show"""case object $name$extendsClause"""
}

final case class CaseClass(
  name: TypeName,
  fields: NonEmptyList[CaseClassField],
  extendsClause: ExtendsClause,
  companionObject: Option[CompanionObject] = None
) extends ScalaModel {

  private val newlineAndCompanion =
    companionObject.foldMap(comp => "\n" + comp.show)

  override def show: String =
    show"""final case class $name(${fields.mkString_("", ", ", "")})$extendsClause$newlineAndCompanion"""
}

final case class Enumerated(
  name: TypeName,
  underlyingType: TypeReference,
  values: NonEmptySet[ScalaLiteral],
  extendsClause: ExtendsClause = ExtendsClause.productWithSerializable
) extends ScalaModel {

  private val inhabitantModels: NonEmptySet[ScalaModel] = values.map { str =>
    CaseObject(str.asTypeName,
               ExtendsClause.appliedType(
                 name,
                 Parameter.fromLiteral(str) :: Nil)): ScalaModel
  }

  override def show: String =
    show"""sealed abstract class $name(value: $underlyingType)$extendsClause
          |
          |${CompanionObject(name, inhabitantModels.toNonEmptyList)}""".stripMargin
}

final case class SealedTraitHierarchy(
  name: TypeName,
  inhabitants: NonEmptyList[ScalaModel],
  discriminator: Option[Discriminator],
  extendsClause: ExtendsClause = ExtendsClause.productWithSerializable
) extends ScalaModel {

  override def show: String =
    show"""sealed trait $name$extendsClause
          |
          |${CompanionObject(name, inhabitants)}""".stripMargin
}
