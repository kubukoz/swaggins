package swaggins.openapi.experimental

import swaggins.openapi.model.OpenAPI
import swaggins.openapi.model.shared._
import cats.implicits._

sealed trait TypeInfo {
  def name: String
}

case class CaseClassField(name: String, tpe: String, required: Boolean) {
  val typeString                = if (required) tpe else s"Option[$tpe]"
  override def toString: String = s"""$name: $typeString"""
}

case class CaseClass(name: String,
                     fields: List[CaseClassField],
                     extending: List[String] = Nil)
    extends TypeInfo {

  val extendingString: String =
    extending.toNel.foldMap(_.toList.mkString(" extends ", " with ", ""))

  override def toString: String =
    s"""case class $name(${fields.mkString(", ")})$extendingString"""
}
case class Alias(name: String, to: String) extends TypeInfo

object Experiment {

  /**
    * Experimental case class generator. To be removed later.
    * */
  def gen(spec: OpenAPI): List[TypeInfo] = {
    spec.components.schemas.toSortedMap.toList.map {
//      case (name, Left(ref)) => Alias(name.value, ref.`$ref`.value)
      case (name, Right(ObjectSchema(required, properties))) =>
        CaseClass(
          toCamel(name.value),
          properties.toList.map { prop =>
            val isRequired =
              required.forall(_.exists(_.value == prop.name.value))
            prop.schema match {
              case Left(ref) =>
                CaseClassField(prop.name.value,
                               referenceString(ref),
                               isRequired)
              case Right(tpe) =>
                CaseClassField(prop.name.value,
                               typeString(tpe)(name.value),
                               isRequired)
            }
          }
        )
      case (name, Right(_: NumberSchema)) =>
        CaseClass(
          toCamel(name.value),
          List(CaseClassField("value", "Int", required = true)),
          List("AnyVal")
        )
      case (name, Right(_: StringSchema)) =>
        CaseClass(
          toCamel(name.value),
          List(CaseClassField("value", "Int", required = true)),
          List("AnyVal")
        )
      case _ => ???
    }
  }

  def typeString(tpe: Schema)(parent: String): String = tpe match {
    case ObjectSchema(_, _) => s"${parent}SyntheticChild1"
    case ArraySchema(items) =>
      s"List[${items.fold(referenceString, typeString)}]"
    case _: NumberSchema => "Int"
    case _: StringSchema => "String"
  }

  def toCamel(s: String): String = {
    s.split("-").toList.map(s => s.head.toUpper + s.tail.toLowerCase).mkString
  }

  def referenceString(ref: Reference): String =
    if (ref.`$ref`.value.startsWith("#/components/schemas/")) {
      val split = ref.`$ref`.value
        .drop("#/components/schemas/".length)
        .split("""/""")
        .toList

      split match {
        case Nil  => ???
        case list => (list.init :+ toCamel(list.last)).mkString(".")
      }
    } else ???
}
