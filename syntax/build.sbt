lazy val protosyntax = project.in(file("../syntax")).settings(
  name := "proto-syntax",
  libraryDependencies += "com.google.protobuf" % "protobuf-java" % "3.15.2",
  scalaVersion := "3.0.0-RC1",
  crossScalaVersions := "3.0.0-RC1" :: "2.13.5" :: Nil,
  resolvers += Resolver.JCenterRepository,
  version := zero.git.version(),
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
)
