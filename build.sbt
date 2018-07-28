import Dependencies.{pureconfig, _}
import sbt.addCompilerPlugin

scalacOptions in ThisBuild ++= Options.flags
scalacOptions in (Compile, console) --= Options.consoleExclusions

enablePlugins(JavaAppPackaging)

val commonDeps = Seq(
  logback,
  log4Cats,
  simulacrum,
  scalatest,
  decline,
  monix
) ++ circe ++ monocle ++ scalacheck ++ pureconfig ++ fs2 ++ cats

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

def makeDep(project: Project): ClasspathDependency =
  project % "compile->compile;test->test"

def veryBasic(proj: Project): Project = proj.settings(commonSettings)

val core    = veryBasic(project)
val coreDep = makeDep(core)

def basic(proj: Project): Project =
  veryBasic(proj).dependsOn(coreDep)

val openapi = basic(project)

val config = basic(project)

val generator = basic(project).dependsOn(openapi, config)

val fetch = basic(project).dependsOn(openapi)

val app = basic(project).dependsOn(generator, config, fetch)

val cli = basic(project).dependsOn(app)

val `swaggins` = (project in file("."))
  .settings(
    mainClass in Compile := Some("swaggins.cli.Main"),
    commonSettings
  )
  .dependsOn(openapi, config, generator, fetch, app, cli)
  .aggregate(openapi, config, generator, fetch, app, cli)
