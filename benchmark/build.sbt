lazy val benchmark = project.in(file(".")).settings(
  libraryDependencies ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 13)) => Seq(
        "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.12.1",
        "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core" % "2.6.4",
        "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % "2.6.4" % "compile-internal",
        "io.suzaku" %% "boopickle" % "1.3.3",
        "com.evolutiongaming" %% "kryo-macros" % "1.3.0",
      )
      case _ => Seq(
        "com.fasterxml.jackson.module" % "jackson-module-scala_2.13" % "2.12.1",
      )
    }
  },
  resolvers += Resolver.bintrayRepo("evolutiongaming", "maven"),
  Compile / PB.targets := Seq(
    scalapb.gen() -> (Compile / sourceManaged).value
  ),
  scalaVersion := "3.0.0-RC1",
  crossScalaVersions := "3.0.0-RC1" :: "2.13.5" :: Nil,
  version := zero.git.version(),
  publish / skip := true,
).dependsOn(protoscala)
 .enablePlugins(JmhPlugin)

lazy val protoscala = project.in(file("../protoscala")).settings(
  name := "proto",
  scalaVersion := "3.0.0-RC1",
  crossScalaVersions := "3.0.0-RC1" :: "2.13.5" :: Nil,
  resolvers += Resolver.JCenterRepository,
  Compile / scalacOptions ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 13)) => Nil
      case _ =>
        Seq(
          "-source", "future-migration"
        , "-deprecation"
        , "-rewrite"
        , "release", "11"
        , "-Yexplicit-nulls"
        )
    }
  },
  version := zero.git.version(),
  libraryDependencies ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 13)) => Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value)
      case _ => Nil
    }
  },
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
).dependsOn(protosyntax)

lazy val protosyntax = project.in(file("../syntax")).settings(
  name := "proto-syntax",
  libraryDependencies += "com.google.protobuf" % "protobuf-java" % "3.15.2",
  scalaVersion := "3.0.0-RC1",
  crossScalaVersions := "3.0.0-RC1" :: "2.13.5" :: Nil,
  resolvers += Resolver.JCenterRepository,
  Compile / scalacOptions ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 13)) => Nil
      case _ =>
        Seq(
          "-source", "future-migration"
        , "-deprecation"
        , "-rewrite"
        , "release", "11"
        , "-Yexplicit-nulls"
        , "-language:strictEquality"
        )
    }
  },
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
