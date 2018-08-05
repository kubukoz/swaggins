package swaggins

import java.nio.file.{Path, Paths}

import monix.eval.Coeval
import org.scalatest.{Matchers, WordSpec}
import swaggins.core.Parser

trait BaseTest extends WordSpec with Matchers {

  def filePath(name: String): Path = {
    Paths.get(getClass.getResource(name).toURI)
  }

  def fileContent(name: String): String = {
    Parser.readFile[Coeval](filePath(name)).value
  }
}
