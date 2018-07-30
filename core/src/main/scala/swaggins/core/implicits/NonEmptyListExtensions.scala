package swaggins.core.implicits
import cats.Order
import cats.data.{NonEmptyList, NonEmptySet}
import cats.implicits._

import scala.collection.immutable.SortedSet

trait NonEmptyListExtensions {
  implicit def nel2Ops[T](nel: NonEmptyList[T]): NelOps[T] = new NelOps(nel)
}

class NelOps[T](private val nel: NonEmptyList[T]) extends AnyVal {

  def toNes(implicit O: Order[T]): NonEmptySet[T] =
    NonEmptySet(nel.head, nel.tail.to[SortedSet])
}
