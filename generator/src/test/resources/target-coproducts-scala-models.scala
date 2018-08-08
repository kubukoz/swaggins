package models

final case class Cat(huntingSkill: Option[HuntingSkill])

sealed abstract class HuntingSkill(value: String) extends Product with Serializable

object HuntingSkill {
  case object Adventurous extends HuntingSkill("adventurous")
  case object Aggressive extends HuntingSkill("aggressive")
  case object Clueless extends HuntingSkill("clueless")
  case object Lazy extends HuntingSkill("lazy")
}

final case class Dog2(value: Dog)

final case class Husky(woof: Option[String])

sealed trait Strnum extends Product with Serializable

object Strnum {
  final case class String(value: scala.Predef.String) extends Strnum
  final case class Double(value: scala.Double) extends Strnum
}

final case class York(woof: Option[String])

sealed trait Pet extends Product with Serializable

object Pet {
  case class Cat(cat: models.Cat) extends Pet
  case class Dog(dog: models.Dog) extends Pet
}

sealed trait Dog extends Product with Serializable

object Dog {
  case class Husky(husky: models.Husky) extends Dog
  case class York(york: models.York) extends Dog
  final case class Anonymous1(name: String, age: Double, gender: Option[Anonymous1.Gender]) extends Dog

  object Anonymous1 {
    sealed abstract class Gender(value: String) extends Product with Serializable
    object Gender {
      case object Female extends Gender("female")
      case object Male extends Gender("male")
    }
  }
}
