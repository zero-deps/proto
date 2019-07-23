ThisBuild / organization := "io.github.zero-deps"
ThisBuild / version := {
  val repo = org.eclipse.jgit.api.Git.open(file("."))
  val desc = repo.describe.setTags(true).call.stripPrefix("v")
  val dirty = if (repo.status.call.isClean) "" else "-dirty"
  s"${desc}${dirty}"
}
ThisBuild / scalaVersion := "2.12.8"
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

lazy val root = project.in(file(".")).settings(
  name := "proto",
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.7" % Test,
  skip in publish := true,
).dependsOn(macros).aggregate(macros, runtime, purs)

lazy val macros = project.in(file("macros")).settings(
  name := "proto-macros",
  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
).dependsOn(runtime)

lazy val runtime = project.in(file("runtime")).settings(
  name := "proto-runtime",
  libraryDependencies += "com.google.protobuf" % "protobuf-java" % "3.7.1",
)

lazy val purs = project.in(file("purs")).settings(
  name := "proto-purs",
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.7" % Test,
  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
).dependsOn(runtime, macros % Test)

lazy val benchmark = project.in(file("benchmark")).settings(
  libraryDependencies += "com.twitter" %% "chill" % "0.9.3",
  libraryDependencies += "org.scodec" %% "core" % "1.10.4",
  libraryDependencies += "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.9.7",
  libraryDependencies += "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core" % "0.36.9" % Compile,
  libraryDependencies += "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % "0.36.9" % Provided,
  PB.targets in Compile := Seq(
    scalapb.gen() -> (sourceManaged in Compile).value
  ),
  skip in publish := true,
).enablePlugins(JmhPlugin).dependsOn(macros)
