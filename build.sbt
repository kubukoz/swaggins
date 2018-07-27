import Dependencies.{pureconfig, _}
import sbt.addCompilerPlugin

scalacOptions in ThisBuild ++= Options.flags
scalacOptions in (Compile, console) --= Options.consoleExclusions

enablePlugins(JavaAppPackaging)

bloopExportJarClassifiers in ThisBuild := Some(Set("sources"))

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

def makeDep(project: Project) = project % "compile->compile;test->test"

val core = project
  .settings(
    commonSettings
  )

val coreDep = makeDep(core)

val `swaggins-cli` = (project in file("."))
  .settings(
    mainClass in Compile := Some("com.kubukoz.swaggins.cli.Main"),
    commonSettings
  )
  .dependsOn(coreDep)
  .aggregate(core)
