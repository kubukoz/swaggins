import sbt._

object Dependencies {

  //plugins
  val splain = "io.tryp" % "splain" % Versions.splain cross CrossVersion.patch

  val macroParadise = ("org.scalamacros" % "paradise" % Versions.macroParadise)
    .cross(CrossVersion.full)
  val kindProjector    = "org.spire-math" %% "kind-projector"     % Versions.kindProjector
  val betterMonadicFor = "com.olegpy"     %% "better-monadic-for" % Versions.bm4
  val scalazDeriving   = "org.scalaz"     %% "deriving-plugin"    % Versions.scalazDeriving

  //libraries
  val scalazDerivingLib = "org.scalaz"           %% "deriving-macro" % Versions.scalazDeriving
  val logback           = "ch.qos.logback"       % "logback-classic" % Versions.logback
  val log4Cats          = "io.chrisdavenport"    %% "log4cats-slf4j" % Versions.log4Cats
  val scalatest         = "org.scalatest"        %% "scalatest"      % Versions.scalatest % Test
  val simulacrum        = "com.github.mpilquist" %% "simulacrum"     % Versions.simulacrum
  val decline           = "com.monovore"         %% "decline"        % Versions.decline
  val chimney           = "io.scalaland"         %% "chimney"        % Versions.chimney

  val scalacheck = Seq(
    "com.fortysevendeg"          %% "scalacheck-datetime"       % Versions.scalacheckJava8 % Test,
    "com.github.alexarchambault" %% "scalacheck-shapeless_1.13" % Versions.scalacheckShapeless
  )

  val pureconfig = Seq(
    "com.github.pureconfig" %% "pureconfig-cats-effect" % Versions.pureconfig,
    "com.github.pureconfig" %% "pureconfig-enumeratum"  % Versions.pureconfig
  )

  val circe = Seq(
    "io.circe" %% "circe-core"           % Versions.circe,
    "io.circe" %% "circe-generic-extras" % Versions.circe,
    "io.circe" %% "circe-parser"         % Versions.circe,
    "io.circe" %% "circe-yaml"           % Versions.circeYaml,
    "io.circe" %% "circe-java8"          % Versions.circe
  )

  val monocle = Seq(
    "com.github.julien-truffaut" %% "monocle-core"  % Versions.monocle,
    "com.github.julien-truffaut" %% "monocle-macro" % Versions.monocle,
    "com.github.julien-truffaut" %% "monocle-law"   % Versions.monocle % Test
  )

  val cats =
    Seq(
      "org.typelevel"     %% "cats-core"    % Versions.cats,
      "io.chrisdavenport" %% "cats-par"     % Versions.catsParTemp,
      "org.typelevel"     %% "kittens"      % Versions.kittens,
      "org.typelevel"     %% "cats-laws"    % Versions.cats % Test,
      "org.typelevel"     %% "cats-testkit" % Versions.cats % Test,
      "com.olegpy"        %% "meow-mtl"     % Versions.meowMtl
    )

  val fs2 =
    Seq(
      "co.fs2" %% "fs2-core" % Versions.fs2,
      "co.fs2" %% "fs2-io"   % Versions.fs2
    )
}

object Versions {
  val splain              = "0.3.5"
  val log4Cats            = "0.2.0"
  val bm4                 = "0.3.0-M4"
  val cats                = "1.5.0"
  val meowMtl             = "0.2.0"
  val catsParTemp         = "0.2.0"
  val circe               = "0.11.0"
  val circeYaml           = "0.9.0"
  val decline             = "0.5.0"
  val kindProjector       = "0.9.9"
  val pureconfig          = "0.10.0"
  val scalatest           = "3.0.5"
  val macroParadise       = "2.1.1"
  val logback             = "1.2.3"
  val simulacrum          = "0.14.0"
  val fs2                 = "1.0.2"
  val monocle             = "1.5.0-cats"
  val scalacheckJava8     = "0.2.0"
  val scalacheckShapeless = "1.1.8"
  val chimney             = "0.3.0"
  val scalazDeriving      = "1.0.0"
  val kittens             = "1.2.0"
}
