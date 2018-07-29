package swaggins.core.implicits
import cats.implicits._

trait StringExtensions {
  implicit def string2SwagginsStringOps(s: String): StringOps = new StringOps(s)
}

class StringOps(private val s: String) extends AnyVal {

  /**
    * Lowercases the string's head, leaves everything else unchanged
    * */
  def lowerHead: String = s.nonEmpty.guard[Option].as(s).foldMap { str =>
    val (head, tail) = (str.head, str.tail)

    show"${head.toLower}$tail"
  }
}
