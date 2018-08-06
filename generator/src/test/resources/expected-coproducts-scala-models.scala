package models

sealed trait Strnum extends Product with Serializable
object Strnum {
  case class String(value: String) extends Strnum
  case class Number(value: Double) extends Strnum
}

sealed trait Pet extends Product with Serializable
object Pet {
  case class Cat(cat: models.Cat) extends Pet
  case class Dog(dog: models.Dog) extends Pet
}

case class Cat(huntingSkill: Option[HuntingSkill])

sealed abstract class HuntingSkill(value: String) extends Product with Serializable

object HuntingSkill {
  case object Clueless extends HuntingSkill("clueless")
  case object Lazy extends HuntingSkill("lazy")
  case object Adventurous extends HuntingSkill("adventurous")
  case object Aggressive extends HuntingSkill("aggressive")
}

sealed trait Dog extends Product with Serializable
object Dog {
  case class Husky(husky: models.Husky) extends Dog
  case class York(york: models.York) extends Dog
}

case class Dog2(value: Dog)

case class Husky(woof: Option[String])
case class York(woof: Option[String])
