val `proto-parent` = project.in(file("."))
  .settings(
    publish / skip := true
  )
  .aggregate(
    proto.jvm, proto.native, proto.js
  , syntax.jvm, syntax.native, syntax.js
  , purs
  , tex
  , bench
  )

val scala = "3.3.3" :: "2.13.14" :: "2.12.20" :: "2.11.12" :: Nil
val protobuf = "4.28.1"
val scalatest = "3.2.19"

ThisBuild / scalaVersion := scala.head
ThisBuild / crossScalaVersions := scala

lazy val proto =
  crossProject(JVMPlatform, NativePlatform, JSPlatform)
    .withoutSuffixFor(JVMPlatform)
    .crossType(CrossType.Pure)
    .dependsOn(syntax)
    .settings(
      name := "proto"
    , libraryDependencies += "com.google.protobuf" % "protobuf-java" % protobuf
    , libraryDependencies += "org.scalatest" %% "scalatest" % scalatest % Test
    , libraryDependencies ++= {
        CrossVersion.partialVersion(scalaVersion.value) match {
          case Some((2, _)) => Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value)
          case _ => Nil
        }
      }
    )
    .nativeSettings(
      crossScalaVersions := scala.take(3)
    )
    .jsSettings(
      crossScalaVersions := scala.take(3)
    )

lazy val purs = project
  .dependsOn(syntax.jvm, proto.jvm % Test)
  .settings(
    name := "proto-purs"
  , libraryDependencies += "org.scalatest" %% "scalatest" % scalatest % Test
  , libraryDependencies ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, 13)) => Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value)
        case _ => Nil
      }
    }
  )

lazy val tex = project
  .dependsOn(syntax.jvm)
  .settings(
    name := "proto-tex"
  , libraryDependencies ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, 13)) => Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value)
        case _ => Nil
      }
    }
  )

lazy val syntax =
  crossProject(JVMPlatform, NativePlatform, JSPlatform)
    .withoutSuffixFor(JVMPlatform)
    .crossType(CrossType.Pure)
    .settings(
      name := "proto-syntax"
    )
    .nativeSettings(
      crossScalaVersions := scala.take(3)
    )
    .jsSettings(
      crossScalaVersions := scala.take(3)
    )

lazy val bench = project
  .dependsOn(proto.jvm)
  .enablePlugins(JmhPlugin)
  .settings(
    publish / skip := true
  , libraryDependencies ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((3, _)) =>
          Seq(
            "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.17.2"
          , "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core" % "2.30.9"
          , "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % "2.30.9" % "compile-internal"
          , "io.suzaku" %% "boopickle" % "1.5.0"
          )
        case _ => Nil
      }
    }
  // , Compile / PB.targets := Seq(scalapb.gen() -> (Compile / sourceManaged).value)
  , scalacOptions := Nil
  )

ThisBuild / organization := "io.github.zero-deps"
ThisBuild / homepage := Some(url("https://github.com/zero-deps/proto"))
ThisBuild / scmInfo := Some(ScmInfo(url("https://github.com/zero-deps/proto"), "git@github.com:zero-deps/proto.git"))
ThisBuild / developers := List(Developer("Z", "D", "zerodeps.org@gmail.com", url("https://github.com/zero-deps")))
ThisBuild / licenses += ("MIT", url("http://opensource.org/licenses/MIT"))
ThisBuild / version := zero.git.version()
ThisBuild / versionScheme := Some("pvp")
ThisBuild / publishTo := Some(Opts.resolver.sonatypeStaging)
ThisBuild / credentials += Credentials("GnuPG Key ID", "gpg", "F68F0EADDB81EF533C4E8E3228C90422E5A0DB21", "ignored")
ThisBuild / isSnapshot := true

ThisBuild / scalacOptions ++= {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, 11)) => Seq(
      "-deprecation"
    , "-nowarn"
    )
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
Global / onChangedBuildSource := ReloadOnSourceChanges
