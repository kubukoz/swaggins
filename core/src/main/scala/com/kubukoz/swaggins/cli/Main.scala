package com.kubukoz.swaggins.cli

import java.nio.file.{Files, Path}

import cats.data.ValidatedNel
import cats.implicits._
import com.kubukoz.swaggins.cli.Spec.Paths
import com.monovore.decline._
import io.circe.Decoder
import io.circe.generic.JsonCodec
import io.circe.yaml.parser

object Main
    extends CommandApp(
      name = "swaggins-cli",
      header = "Welcome to swaggins-cli!",
      main = {
        val userOpt =
          Opts.option[Path]("spec", help = "Specification file.").mapValidated(MainUtils.validateSpec)

        val silent = Opts.flag("silent", help = "Silent mode.").orFalse

        (userOpt, silent).mapN { (spec, silent) =>
          if (silent) println("...")
          else {
            println(s"Hello $spec!")
            spec.paths.paths.foreach(println)
          }
        }
      }
    )

object MainUtils {
  import io.circe.yaml.syntax._

  def validateSpec(path: Path): ValidatedNel[String, Spec] =
    path.valid
      .ensure("spec must be a file.")(_.toFile.exists())
      .map(Files.newBufferedReader)
      .andThen(parser.parse(_).toValidated.leftMap(_.message))
      .andThen(_.as[Spec].toValidated.leftMap(_.message))
      .toValidatedNel
}

import io.circe.generic.extras.semiauto._

@JsonCodec(decodeOnly = true)
case class Spec(paths: Paths)

object Spec {
  case class Paths(paths: List[Path])

  object Paths {
    implicit val decoder: Decoder[Paths] = for {
      obj <- Decoder.decodeJsonObject
      paths = obj.toMap.mapValues(_.as[PathBody]).map { case (k, vRes) => vRes.map(Path(k, _)) }.toList
      result <- Decoder.instance { _ =>
        paths.sequence[Decoder.Result, Path].map(Paths(_))
      }
    } yield result
  }

  @JsonCodec
  case class PathBody()

  case class Path(path: String, body: PathBody)
}
