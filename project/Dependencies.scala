import sbt._

object Dependencies {
  //plugins
  val macroParadise    = ("org.scalamacros" % "paradise" % Versions.macroParadise).cross(CrossVersion.full)
  val kindProjector    = "org.spire-math" %% "kind-projector" % Versions.kindProjector
  val betterMonadicFor = "com.olegpy" %% "better-monadic-for" % Versions.bm4

  //libraries
  val logback  = "ch.qos.logback"    % "logback-classic" % Versions.logback
  val log4Cats = "io.chrisdavenport" %% "log4cats-slf4j" % Versions.log4Cats

  val scalatest  = "org.scalatest"        %% "scalatest"  % Versions.scalatest % Test
  val simulacrum = "com.github.mpilquist" %% "simulacrum" % Versions.simulacrum
  val monix      = "io.monix"             %% "monix"      % Versions.monix
  val decline    = "com.monovore"         %% "decline"    % Versions.decline

  val scalacheck = Seq(
    "com.fortysevendeg"          %% "scalacheck-datetime"       % Versions.scalacheckJava8 % Test,
    "com.github.alexarchambault" %% "scalacheck-shapeless_1.13" % Versions.scalacheckShapeless
  )

  val pureconfig = Seq(
    "com.github.pureconfig" %% "pureconfig-cats-effect" % Versions.pureconfig,
    "com.github.pureconfig" %% "pureconfig-enumeratum"  % Versions.pureconfig
  )

  val macwire = Seq(
    "com.softwaremill.macwire" %% "macros" % Versions.macwire % Provided,
    "com.softwaremill.macwire" %% "util"   % Versions.macwire,
    "com.softwaremill.macwire" %% "proxy"  % Versions.macwire
  )

  val circe = Seq(
    "io.circe" %% "circe-core"           % Versions.circe,
    "io.circe" %% "circe-generic-extras" % Versions.circe,
    "io.circe" %% "circe-parser"         % Versions.circe,
    "io.circe" %% "circe-java8"          % Versions.circe
  )

  val monocle = Seq(
    "com.github.julien-truffaut" %% "monocle-core"  % Versions.monocle,
    "com.github.julien-truffaut" %% "monocle-macro" % Versions.monocle,
    "com.github.julien-truffaut" %% "monocle-law"   % Versions.monocle % Test
  )

  val cats =
    Seq(
      "io.chrisdavenport" %% "cats-par"     % Versions.catsParTemp,
      "org.typelevel"     %% "cats-laws"    % Versions.cats % Test,
      "org.typelevel"     %% "cats-testkit" % Versions.cats % Test
    )
}

object Versions {
  val log4Cats            = "0.0.7"
  val bm4                 = "0.2.4"
  val cats                = "1.1.0"
  val catsParTemp         = "0.1.0"
  val circe               = "0.9.3"
  val decline             = "0.4.2"
  val kindProjector       = "0.9.6"
  val pureconfig          = "0.9.1"
  val scalatest           = "3.0.5"
  val macroParadise       = "2.1.1"
  val macwire             = "2.3.1"
  val logback             = "1.2.3"
  val simulacrum          = "0.12.0"
  val monix               = "3.0.0-RC1"
  val monocle             = "1.5.0"
  val scalacheckJava8     = "0.2.0"
  val scalacheckShapeless = "1.1.8"
}
