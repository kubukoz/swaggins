package swaggins.scala.ast.model

import cats.data.{NonEmptyList, NonEmptyMap, NonEmptySet}
import cats.implicits._
import cats.{Order, Show}
import swaggins.core.implicits._
import swaggins.scala.ast.ref.{OrdinaryType, TypeName, TypeReference}

sealed trait ScalaModel extends Product with Serializable {
  type Self <: ScalaModel
  def show: String
  def setExtendsClause(extendsClause: ExtendsClause): Self
}

object ScalaModel {
  implicit val show: Show[ScalaModel] = _.show
}

case class CompanionObject(models: List[ScalaModel]) {
  def show(name: TypeName): String = show"""object $name {
  |${models.mkString_("", "\n", "").indented(2)}
  |}""".stripMargin
}

case class CaseClass(name: TypeName,
                     fields: NonEmptyList[CaseClassField],
                     extendsClause: ExtendsClause,
                     companionObject: Option[CompanionObject] = None)
    extends ScalaModel {

  override type Self = CaseClass

  override def setExtendsClause(extendsClause: ExtendsClause): CaseClass =
    copy(extendsClause = extendsClause)

  private val newlineAndCompanion =
    companionObject.foldMap(comp => "\n" + comp.show(name))
  override def show: String =
    show"""final case class $name(${fields.mkString_("", ", ", "")})$extendsClause$newlineAndCompanion"""
}

object ValueClass {

  def apply(name: TypeName, tpe: TypeReference): CaseClass =
    CaseClass(name,
              NonEmptyList.one(
                CaseClassField(required = true, FieldName("value"), tpe)),
              ExtendsClause.anyVal)
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

case class ExtendsClause(refs: List[TypeReference]) extends AnyVal

object ExtendsClause {
  implicit val show: Show[ExtendsClause] = _.refs match {
    case Nil  => ""
    case refs => refs.mkString_(" extends ", " with ", "")
  }

  val empty: ExtendsClause = ExtendsClause(Nil)

  val productWithSerializable = ExtendsClause(
    List(OrdinaryType("Product"), OrdinaryType("Serializable")))

  val anyVal = ExtendsClause(List(OrdinaryType("AnyVal")))
}

case class SealedTraitHierarchy(name: TypeName,
                                inhabitants: NonEmptyList[ScalaModel],
                                discriminator: Option[Discriminator],
                                extendsClause: ExtendsClause =
                                  ExtendsClause.productWithSerializable)
    extends ScalaModel {

  override type Self = SealedTraitHierarchy
  private val inhabitantsShow: String =
    inhabitants.map(_.show).mkString_("", "\n", "")

  override def setExtendsClause(
    extendsClause: ExtendsClause): SealedTraitHierarchy =
    copy(extendsClause = extendsClause)

  override def show: String =
    show"""sealed trait $name$extendsClause
          |
          |object $name {
          |${inhabitantsShow.indented(2)}
          |}""".stripMargin
}

case class Discriminator(propertyName: Option[FieldName],
                         mapping: Option[NonEmptyMap[String, OrdinaryType]])

sealed trait ScalaLiteral extends Product with Serializable {

  def asTypeName: TypeName = this match {
    case ScalaLiteral.String(value) => TypeName.parse(value)
    case ScalaLiteral.Double(value) => TypeName.raw(show"`$value`")
  }
}

object ScalaLiteral {
  case class String(value: scala.Predef.String) extends ScalaLiteral

  case class Double(value: scala.Double) extends ScalaLiteral

  object String {
    implicit val show: Show[String]   = _.value
    implicit val order: Order[String] = Order.by(_.value)
  }
}

case class Enumerated[Literal <: ScalaLiteral: Show](
  name: TypeName,
  underlyingType: TypeReference,
  values: NonEmptySet[Literal],
  extendsClause: ExtendsClause = ExtendsClause.productWithSerializable)
    extends ScalaModel {

  override type Self = Enumerated[Literal]
  private val inhabitantsShow: String = values.map { str =>
    show"""case object ${str.asTypeName} extends $name("$str")"""
  }.mkString_("", "\n", "")

  override def setExtendsClause(
    extendsClause: ExtendsClause): Enumerated[Literal] =
    copy(extendsClause = extendsClause)

  override def show: String =
    show"""sealed abstract class $name(value: $underlyingType)$extendsClause
          |
          |object $name {
          |${inhabitantsShow.indented(2)}
          |}""".stripMargin
}
