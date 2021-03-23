lazy val protoscala = project.in(file(".")).settings(
  name := "proto-scala",
  version := zero.git.version(),
  scalaVersion := "3.0.0-RC1",
  crossScalaVersions := "3.0.0-RC1" :: "2.13.5" :: Nil,
  resolvers += Resolver.JCenterRepository,
  libraryDependencies ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 13)) => Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value)
      case _ => Nil
    }
  },
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.5" % Test,
  /* publishing */
  organization := "io.github.zero-deps",
  homepage := Some(url("https://github.com/zero-deps/proto")),
  scmInfo := Some(ScmInfo(url("https://github.com/zero-deps/proto"), "git@github.com:zero-deps/proto.git")),
  developers := List(Developer("Zero", "Deps", "zerodeps.org@gmail.com", url("https://github.com/zero-deps"))),
  licenses += ("MIT", url("http://opensource.org/licenses/MIT")),
  publishMavenStyle := true,
  versionScheme := Some("pvp"),
  publishTo := Some(Opts.resolver.sonatypeStaging),
  usePgpKeyHex("F68F0EADDB81EF533C4E8E3228C90422E5A0DB21"),
  /* publishing */
).dependsOn(protoops).aggregate(protosyntax, benchmark, protopurs, prototex)

lazy val protosyntax = project.in(file("syntax"))
lazy val benchmark = project.in(file("benchmark"))
lazy val protopurs = project.in(file("purs"))
lazy val prototex = project.in(file("tex"))
lazy val protoops = project.in(file("ops"))

turbo := true
useCoursier := true
Global / onChangedBuildSource := ReloadOnSourceChanges
