package swaggins

import java.nio.file.{Path, Paths}

import org.scalatest.{Matchers, WordSpec}

trait BaseTest extends WordSpec with Matchers {

  def filePath(name: String): Path = {
    Paths.get(getClass.getResource(name).toURI)
  }
}
