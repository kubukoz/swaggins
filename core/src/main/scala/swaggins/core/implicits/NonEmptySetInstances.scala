package swaggins.core.implicits
import cats.data.{NonEmptyList, NonEmptySet}
import cats.implicits._
import cats.kernel.Order
import io.circe.Decoder

import scala.collection.immutable.SortedSet

trait NonEmptySetInstances {
  implicit def nesDecoder[T: Decoder: Order]: Decoder[NonEmptySet[T]] =
    Decoder[NonEmptyList[T]].map { nel =>
      NonEmptySet(nel.head, nel.tail.to[SortedSet])
    }
}
