package com.kubukoz.swaggins.cli

import java.nio.file.{Files, Path}

import cats.data.ValidatedNel
import cats.implicits._
import com.kubukoz.swaggins.cli.spec.Spec
import com.monovore.decline._
import io.circe.yaml.parser

object Main
    extends CommandApp(
      name = "swaggins-cli",
      header = "Welcome to swaggins-cli!",
      main = {
        val parsedSpec =
          Opts.option[Path]("spec", help = "Specification file.").mapValidated(MainUtils.parseSpec)

        val silent = Opts.flag("silent", help = "Silent mode.").orFalse

        (parsedSpec, silent).mapN { (spec, silent) =>
          if (silent) println("...")
          else {
            println(s"Hello $spec!")
            spec.paths.paths.foreach(println)
          }
        }
      }
    )

object MainUtils {

  def parseSpec(path: Path): ValidatedNel[String, Spec] =
    path.valid
      .ensure("spec must be a file.")(_.toFile.exists())
      .map(Files.newBufferedReader)
      .andThen(parser.parse(_).toValidated.leftMap(_.message))
      .andThen(_.as[Spec].toValidated.leftMap(_.message))
      .toValidatedNel
}
