import Dependencies.{pureconfig, _}
import sbt.addCompilerPlugin

scalacOptions in ThisBuild ++= Options.flags
scalacOptions in (Compile, console) --= Options.consoleExclusions

enablePlugins(JavaAppPackaging)

val commonDeps = Seq(
  logback,
  log4Cats,
  simulacrum,
  fs2,
  scalatest,
  decline,
  monix
) ++ circe ++ monocle ++ scalacheck ++ pureconfig

val plugins = List(
  addCompilerPlugin(macroParadise),
  addCompilerPlugin(kindProjector),
  addCompilerPlugin(betterMonadicFor)
)

val commonSettings = Seq(
  organization := "com.kubukoz",
  scalaVersion := "2.12.6",
  version := "0.0.1",
  libraryDependencies ++= commonDeps,
  (Test / fork) := true
) ++ plugins

def makeDep(project: Project): ClasspathDependency = project % "compile->compile;test->test"

val openapi = project
  .settings(
    commonSettings
  )

val config = project
  .settings(
    commonSettings
  )

val generator = project
  .settings(
    commonSettings
  )

val fetch = project
  .settings(
    commonSettings
  )

val cli = project
  .settings(
    commonSettings
  )
  .dependsOn(openapi)
  .aggregate(openapi)

val `swaggins` = (project in file("."))
  .settings(
    mainClass in Compile := Some("com.kubukoz.swaggins.cli.Main"),
    commonSettings
  )
  .dependsOn(openapi, config, generator, fetch, cli)
  .aggregate(openapi, config, generator, fetch, cli)
