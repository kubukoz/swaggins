package swaggins.core.implicits
import cats.data.NonEmptyMap
import cats.implicits._
import cats.kernel.Order
import io.circe.{Decoder, KeyDecoder}

import scala.collection.immutable.SortedMap

trait NonEmptyMapInstances {
  implicit def nemDecoder[K: KeyDecoder: Order, V: Decoder]
    : Decoder[NonEmptyMap[K, V]] = Decoder[SortedMap[K, V]].emap {
    NonEmptyMap.fromMap(_).toRight("The map was empty")
  }

  //probably unlawful but... yeah
  implicit def nemOrder[K: Order, V]: Order[NonEmptyMap[K, V]] =
    Order.by(_.keys.toList)
}
