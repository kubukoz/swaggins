package swaggins.cli

import java.nio.file.Path
import cats.implicits._
import com.monovore.decline._

object Main
    extends CommandApp(
      name = "swaggins-cli",
      header = "Welcome to swaggins-cli!",
      main = {
        val parsedSpec = Opts.option[Path]("spec", help = "Specification file.")
        parsedSpec.void
      }
    )
