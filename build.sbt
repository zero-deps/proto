ThisBuild / organization := "io.github.zero-deps"
ThisBuild / version := zd.gs.git.GitOps.version
ThisBuild / scalaVersion := "2.13.1"
ThisBuild / scalacOptions ++= Seq(
  "-Ywarn-extra-implicit",
  "-Xfatal-warnings",
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Ywarn-unused:implicits",
  "-Ywarn-unused:imports",
  "-Yno-completion",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard"
)
ThisBuild / licenses += ("MIT", url("http://opensource.org/licenses/MIT"))
ThisBuild / isSnapshot := true

ThisBuild / turbo := true
ThisBuild / useCoursier := true
Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val purs = project.in(file(".")).settings(
  name := "proto-purs",
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.0-RC3" % Test,
  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  libraryDependencies += "io.github.zero-deps" %% "proto-macros" % "1.7.1" % Test,
  libraryDependencies += "io.github.zero-deps" %% "proto-runtime" % "1.7.1",
)
