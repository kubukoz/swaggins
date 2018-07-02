package com.kubukoz.swaggins.cli

import java.nio.file.{Files, Path}

import enumeratum._
import cats.data.ValidatedNel
import cats.implicits._
import com.kubukoz.swaggins.cli.Spec.Paths
import com.monovore.decline._
import io.circe.{Decoder, KeyDecoder}
import io.circe.generic.JsonCodec
import io.circe.yaml.parser

import scala.collection.immutable

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
    implicit val decoder: Decoder[Paths] = Decoder.decodeJsonObject(_).flatMap { obj =>
      val paths = obj.toMap.mapValues(_.as[Methods]).toList.traverse[Decoder.Result, Path] {
        case (k, bodyResult) => bodyResult.map(Path(k, _))
      }

      paths.map(Paths(_))
    }
  }

  @JsonCodec
  case class PathBody()

  type Methods = Map[HttpMethod, PathBody]
  case class Path(path: String, methods: Methods)

  sealed trait HttpMethod extends EnumEntry with  Product with Serializable
  object HttpMethod extends Enum[HttpMethod] {
    case object Get extends HttpMethod
    case object Post extends HttpMethod

    override def values: immutable.IndexedSeq[HttpMethod] = findValues

    implicit val decoder: KeyDecoder[HttpMethod] = KeyDecoder.instance(HttpMethod.withNameInsensitiveOption)
  }
}
