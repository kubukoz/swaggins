package swaggins.core.implicits
import cats.implicits._

trait StringExtensions {
  implicit def string2SwagginsStringOps(s: String): StringOps = new StringOps(s)
}

final class StringOps(private val s: String) extends AnyVal {

  /**
    * Lowercases the string's head, leaves everything else unchanged
    * */
  def lowerHead: String = modHead(_.toLower)

  def toCamelCase: String = {
    val afterSplit = s.split("\\-").toList
    afterSplit.foldMap(_.upperHead)
  }

  /**
    * Uppercases the string's head, leaves everything else unchanged
    * */
  def upperHead: String = modHead(_.toUpper)

  /**
    * Modifies the head (if present), leaves everything else unchanged.
    * For empty string it's identity.
    * */
  def modHead(f: Char => Char): String = {
    s.nonEmpty.guard[Option].as(s).foldMap { str =>
      val (head, tail) = (str.head, str.tail)

      show"${f(head)}$tail"
    }
  }

  def toCamelCaseLower: String = {
    val afterSplit = s.split("\\-").toList
    afterSplit match {
      case h :: t => h.toLowerCase + t.foldMap(_.upperHead)
      case _      => ""
    }
  }

  def indented(columns: Int): String = {
    s.lines
      .map {
        case line if line.trim.nonEmpty => " " * columns + line
        case _                          => ""
      }
      .mkString("\n")
  }
}
