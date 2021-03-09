lazy val protopurs = project.in(file(".")).settings(
  libraryDependencies ++= deps
, resolvers += Resolver.jcenterRepo
, resolvers += Resolver.githubPackages("zero-deps")
, scalacOptions ++= opts
, scalaVersion := "2.13.5"
, turbo := true
, useCoursier := true
, Global / onChangedBuildSource := ReloadOnSourceChanges
)

val deps = Seq(
  "org.scala-lang" % "scala-reflect" % "2.13.5"
, "io.github.zero-deps" %% "proto-runtime" % "1.8"
, "io.github.zero-deps" %% "proto-macros"  % "1.8" % Test
, "io.github.zero-deps" %% "ext" % "2.3.1.g6719341"
, "org.scalatest" %% "scalatest" % "3.1.1" % Test
)

val opts = Seq(
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
