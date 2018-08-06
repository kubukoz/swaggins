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
  final case class String(value: String) extends Strnum
  final case class Double(value: Double) extends Strnum
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
}
