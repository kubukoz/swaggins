package models

final case class Cat(huntingSkill: Option[HuntingSkill])

sealed abstract class HuntingSkill(value: String) extends Product with Serializable

object HuntingSkill {
  case object Adventurous extends HuntingSkill("adventurous")
  case object Aggressive extends HuntingSkill("aggressive")
  case object Clueless extends HuntingSkill("clueless")
  case object Lazy extends HuntingSkill("lazy")
}

sealed trait Dog extends Product with Serializable

object Dog {
  final case class SYNTHETIC_NAME(value: Husky)
  final case class SYNTHETIC_NAME(value: York)
}

final case class Dog2(value: Dog)

final case class Husky(woof: Option[String])

sealed trait Pet extends Product with Serializable

object Pet {
  final case class SYNTHETIC_NAME(value: Cat)
  final case class SYNTHETIC_NAME(value: Dog)
}

sealed trait Strnum extends Product with Serializable

object Strnum {
  final case class SYNTHETIC_NAME(value: String) extends AnyVal
  final case class SYNTHETIC_NAME(value: Double) extends AnyVal
}

final case class York(woof: Option[String])
