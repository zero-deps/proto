val `proto-parent` = project.in(file(".")).settings(
  publish / skip := true
, version := zero.git.version()
).aggregate(proto, protosyntax, protopurs, prototex, protoops, bench)

ThisBuild / scalaVersion := "3.3.3"
ThisBuild / crossScalaVersions := "3.3.3" :: "2.13.13" :: "2.12.19" :: Nil

lazy val proto = project.in(file("proto")).settings(
  name := "proto",
  version := zero.git.version(),
  libraryDependencies += "com.google.protobuf" % "protobuf-java" % "3.25.3",
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.18" % Test
).dependsOn(protoops)

lazy val protopurs = project.in(file("purs")).settings(
  name := "proto-purs"
, version := zero.git.version()
, libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.18" % Test
).dependsOn(protoops, proto % Test)

lazy val prototex = project.in(file("tex")).settings(
  name := "proto-tex"
, version := zero.git.version()
).dependsOn(protoops)

lazy val protoops = project.in(file("ops")).settings(
  name := "proto-ops"
, version := zero.git.version()
, libraryDependencies ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 13)) => Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value)
      case Some((2, 12)) => Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value)
      case _ => Nil
    }
  }
).dependsOn(protosyntax)

lazy val protosyntax = project.in(file("syntax")).settings(
  name := "proto-syntax"
, version := zero.git.version()
)

lazy val bench = project.in(file("bench")).settings(
  publish / skip := true
, libraryDependencies += "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.16.1"
, libraryDependencies ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 13)) => Seq(
        "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core" % "2.28.3",
        "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % "2.28.3" % "compile-internal",
        "io.suzaku" %% "boopickle" % "1.4.0",
      )
      case _ => Nil
    }
  }
// , Compile / PB.targets := Seq(scalapb.gen() -> (Compile / sourceManaged).value)
, scalacOptions := Nil
).dependsOn(proto).enablePlugins(JmhPlugin)

ThisBuild / organization := "io.github.zero-deps"
ThisBuild / homepage := Some(url("https://github.com/zero-deps/proto"))
ThisBuild / scmInfo := Some(ScmInfo(url("https://github.com/zero-deps/proto"), "git@github.com:zero-deps/proto.git"))
ThisBuild / developers := List(Developer("Z", "D", "zerodeps.org@gmail.com", url("https://github.com/zero-deps")))
ThisBuild / licenses += ("MIT", url("http://opensource.org/licenses/MIT"))
ThisBuild / versionScheme := Some("pvp")
ThisBuild / publishTo := Some(Opts.resolver.sonatypeStaging)
ThisBuild / credentials += Credentials("GnuPG Key ID", "gpg", "F68F0EADDB81EF533C4E8E3228C90422E5A0DB21", "ignored")
ThisBuild / isSnapshot := true

ThisBuild / scalacOptions ++= {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, 12)) => Seq(
      "-deprecation"
    , "-Wconf:cat=deprecation&msg=Auto-application:silent"
    )
    case Some((2, 13)) => Seq(
      "-deprecation"
    , "-Wconf:cat=deprecation&msg=Auto-application:silent"
    )
    case _ => Seq(
      "-source", "future-migration", "-deprecation"
    , "release", "11"
    , "-Yexplicit-nulls"
    , "-Xfatal-warnings"
    , "-Wunused:imports"
    // , "-Xcheck-macros"
    )
  }
}

turbo := true
useCoursier := true
Global / onChangedBuildSource := ReloadOnSourceChanges
