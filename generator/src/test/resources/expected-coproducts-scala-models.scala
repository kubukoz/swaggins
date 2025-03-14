package models

final case class Cat(huntingSkill: _root_.scala.Option[models.Cat.HuntingSkill])
object Cat {
  sealed abstract class HuntingSkill(value: _root_.scala.Predef.String) extends _root_.scala.Product with _root_.scala.Serializable
  object HuntingSkill {
    case object Adventurous extends models.Cat.HuntingSkill("adventurous")
    case object Aggressive extends models.Cat.HuntingSkill("aggressive")
    case object Clueless extends models.Cat.HuntingSkill("clueless")
    case object Lazy extends models.Cat.HuntingSkill("lazy")
  }
}

sealed trait Dog extends _root_.scala.Product with _root_.scala.Serializable
object Dog {
  final case class Husky(value: models.Dog.Husky) extends models.Dog
  final case class York(value: models.Dog.York) extends models.Dog
  final case class Anonymous$1(name: _root_.scala.Predef.String, age: _root_.scala.Double, gender: _root_.scala.Option[models.Dog.Anonymous$1.Gender]) extends models.Dog
  object Anonymous$1 {
    sealed abstract class Gender(value: _root_.scala.Predef.String) extends _root_.scala.Product with _root_.scala.Serializable
    object Gender {
      case object Female extends models.Dog.Anonymous$1.Gender("female")
      case object Male extends models.Dog.Anonymous$1.Gender("male")
    }
  }
}

final case class Dog2(value: models.Dog)

final case class Husky(woof: _root_.scala.Option[_root_.scala.Predef.String])

sealed trait Pet extends _root_.scala.Product with _root_.scala.Serializable
object Pet {
  final case class Cat(value: models.Pet.Cat) extends models.Pet
  final case class Dog(value: models.Pet.Dog) extends models.Pet
}

sealed trait Strnum extends _root_.scala.Product with _root_.scala.Serializable
object Strnum {
  final case class String(value: _root_.scala.Predef.String) extends models.Strnum
  final case class Double(value: _root_.scala.Double) extends models.Strnum
}

final case class York(woof: _root_.scala.Option[_root_.scala.Predef.String])
