package swaggins.scala.ast.model

import cats.data.{NonEmptyList, NonEmptyMap, NonEmptySet}
import cats.implicits._
import cats.{Order, Show}
import swaggins.core.implicits._
import swaggins.scala.ast.ref.{OrdinaryType, TypeName, TypeReference}

sealed trait ScalaModel extends Product with Serializable {
  def show: String
}

case class CaseClass(name: TypeName,
                     fields: NonEmptyList[CaseClassField],
                     extendsClauses: Option[ExtendsClauses])
    extends ScalaModel {

  override def show: String =
    show"""final case class $name(${fields.mkString_("", ", ", "")})${extendsClauses
      .foldMap(_.show)}"""
}

object ValueClass {

  def apply(name: TypeName, tpe: TypeReference): CaseClass =
    CaseClass(name,
              NonEmptyList.one(
                CaseClassField(required = true, FieldName("value"), tpe)),
              Some(ExtendsClauses.anyVal))
}

case class CaseClassField(required: Boolean,
                          name: FieldName,
                          tpe: TypeReference) {

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

case class FieldName(value: String) extends AnyVal

object FieldName {
  implicit val show: Show[FieldName] = Show.show(_.value.toCamelCaseLower)
}

case class ExtendsClauses(refs: NonEmptyList[TypeReference]) extends AnyVal

object ExtendsClauses {
  implicit val show: Show[ExtendsClauses] = Show.show { clauses =>
    show""" extends ${clauses.refs.mkString_("", " with ", "")}"""
  }

  val productWithSerializable = ExtendsClauses(
    NonEmptyList.of(OrdinaryType("Product"), OrdinaryType("Serializable")))

  val anyVal = ExtendsClauses(NonEmptyList.one(OrdinaryType("AnyVal")))
}
case class SealedTraitHierarchy(name: TypeName,
                                inhabitants: NonEmptyList[ScalaModel],
                                discriminator: Option[Discriminator])
    extends ScalaModel {

  private val extendsClauses: Option[ExtendsClauses] = Some(
    ExtendsClauses.productWithSerializable)

  private val inhabitantsShow: String =
    inhabitants.map(_.show).mkString_("", "\n", "")

  override def show: String =
    show"""sealed trait $name${extendsClauses.foldMap(_.show)}
          |object $name {
          |${inhabitantsShow.indented(2)}
          |}""".stripMargin
}

case class Discriminator(propertyName: Option[FieldName],
                         mapping: Option[NonEmptyMap[String, OrdinaryType]])

sealed trait ScalaLiteral extends Product with Serializable {

  def asTypeName: TypeName = this match {
    case ScalaLiteral.String(value) => TypeName(value)
    case ScalaLiteral.Double(value) => TypeName(show"`$value`")
  }
}

object ScalaLiteral {
  case class String(value: scala.Predef.String) extends ScalaLiteral

  object String {
    implicit val show: Show[String]   = _.value
    implicit val order: Order[String] = Order.by(_.value)
  }

  case class Double(value: scala.Double) extends ScalaLiteral
}

case class Enumerated[Literal <: ScalaLiteral: Show](
  name: TypeName,
  underlyingType: TypeReference,
  values: NonEmptySet[Literal])
    extends ScalaModel {
  private val extendsClauses: Option[ExtendsClauses] = Some(
    ExtendsClauses.productWithSerializable)

  private val inhabitantsShow: String = values.map { str =>
    show"""case object ${str.asTypeName} extends $name("$str")"""
  }.mkString_("", "\n", "")

  override def show: String =
    show"""sealed abstract class $name(value: $underlyingType)${extendsClauses
            .foldMap(_.show)}
          |object $name {
          |${inhabitantsShow.indented(2)}
          |}""".stripMargin
}
