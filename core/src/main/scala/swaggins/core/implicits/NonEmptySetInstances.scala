package swaggins.core.implicits

import cats.Order
import cats.data.NonEmptySet
import cats.implicits._

trait NonEmptySetInstances {
  implicit def nesOrder[T: Order]: Order[NonEmptySet[T]] =
    Order.by(_.toSortedSet)
}
