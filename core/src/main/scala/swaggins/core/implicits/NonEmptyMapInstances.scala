package swaggins.core.implicits
import cats.data.NonEmptyMap
import cats.implicits._
import cats.kernel.Order

trait NonEmptyMapInstances {
  //probably unlawful but... yeah
  implicit def nemOrder[K: Order, V]: Order[NonEmptyMap[K, V]] =
    Order.by(_.keys.toList)
}
