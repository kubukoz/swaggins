package com.kubukoz.swaggins.cli

import cats.implicits._
import com.monovore.decline._

object Main extends CommandApp (
  name = "swaggins-cli",
  header = "Welcome to swaggins-cli!",
  main = {
    val userOpt =
      Opts.option[String]("spec", help = "Specification file.")

    val silent = Opts.flag("silent", help = "Silent mode.").orFalse

    (userOpt, silent).mapN { (spec, silent) =>
      if (silent) println("...")
      else println(s"Hello $spec!")
    }
  }
)
