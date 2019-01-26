package swaggins.scala.ast.model

import cats.data.{NonEmptyList, NonEmptySet}
import cats.implicits._
import cats.{Order, Show}
import monocle._
import scalaz.deriving
import swaggins.core.implicits._
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

  def enumeration(
    name: TypeName,
    underlyingType: TypeReference,
    values: NonEmptySet[ScalaLiteral]
  ): ModelWithCompanion = {

    val inhabitantModels: NonEmptySet[ScalaModel] = values.map { str =>
      ScalaModel.caseObject(
        str.asTypeName,
        ExtendsClause.appliedType(TypeReference.byName(name),
                                  Parameter.fromLiteral(str) :: Nil),
        Body.empty): ScalaModel
    }

    /**
      * The base class for enum implementations
      * */
    val cls = Klass(
      Modifiers.of(Modifier.Sealed, Modifier.Abstract),
      name,
      Constructor.withFields(ClassField(FieldName("value"), underlyingType)),
      ExtendsClause.productWithSerializable
    )

    /**
      * The object with enum implementations
      * */
    val obj = SingletonObject(Modifiers.empty,
                              name,
                              ExtendsClause.empty,
                              Body.models(inhabitantModels.toList))

    ModelWithCompanion.both(cls, obj)
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

@deriving(Order)
case class Constructor(fields: NonEmptyList[ClassField])

object Constructor {

  def withFields(field1: ClassField, fields: ClassField*): Constructor =
    Constructor(NonEmptyList(field1, fields.toList))

  implicit val show: Show[Constructor] = cons =>
    cons.fields.mkString_("(", ", ", ")")
}

@deriving(Order)
final case class ClassField(
  name: FieldName,
  tpe: TypeReference
)

object ClassField {
  implicit val show: Show[ClassField] = field =>
    show"""${field.name}: ${field.tpe}"""

  def optional(name: FieldName, typeReference: TypeReference): ClassField =
    ClassField(name, TypeReference.optional(typeReference))
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

  def appliedType(ref: TypeReference, params: List[Parameter]): ExtendsClause =
    single(AppliedType(ref, Nil, params))
}

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

@deriving(Order)
case class Modifiers(value: List[Modifier]) extends AnyVal

object Modifiers {
  val empty: Modifiers               = Modifiers(Nil)
  def of(mods: Modifier*): Modifiers = Modifiers(mods.toList)

  implicit val show: Show[Modifiers] = {
    case `empty`         => ""
    case Modifiers(mods) => mods.mkString_("", " ", " ")
  }
}

@deriving(Order)
sealed trait Statement extends Product with Serializable

object Statement {
  case class ModelStatement(model: ScalaModel) extends Statement

  def model(underlying: ScalaModel): Statement = ModelStatement(underlying)

  implicit val show: Show[Statement] = {
    case ModelStatement(model) => model.show
  }
}

@deriving(Order)
final case class Body(statements: List[Statement])

object Body {
  val empty: Body = Body(Nil)

  def models(models: List[ScalaModel]): Body = Body(models.map(Statement.model))
}
