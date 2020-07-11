organization := "io.github.zero-deps"
version := zero.ext.git.version
scalaVersion := "2.13.3"
scalacOptions ++= Seq(
    "-Ywarn-extra-implicit"
  , "-Xfatal-warnings"
  , "-deprecation"
  , "-feature"
  , "-unchecked"
  , "-Ywarn-unused:implicits"
  , "-Ywarn-unused:imports"
  , "-Yno-completion"
  , "-Ywarn-numeric-widen"
  , "-Ywarn-value-discard"
  , "-Xmaxerrs", "1"
  , "-Xmaxwarns", "3"
  , "-Wconf:cat=deprecation&msg=Auto-application:silent"
)
licenses += "MIT" -> url("http://opensource.org/licenses/MIT")
isSnapshot := true
resolvers += Resolver.bintrayRepo("zero-deps", "maven")

ThisBuild / turbo := true
ThisBuild / useCoursier := true
Global / onChangedBuildSource := ReloadOnSourceChanges

name := "proto-purs"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.1" % Test
libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
libraryDependencies += "io.github.zero-deps" %% "proto-macros" % "1.7.1-2-gf29fcc7" % Test
libraryDependencies += "io.github.zero-deps" %% "proto-runtime" % "1.7.1-2-gf29fcc7"
libraryDependencies += "io.github.zero-deps" %% "ext" % "2.2.0-5-g1b0be7f"
