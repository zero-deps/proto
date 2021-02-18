// ThisBuild / organization := "io.github.zero-deps"
// ThisBuild / version := zero.git.version()
// ThisBuild / scalacOptions ++= Seq(
//   // "-Ywarn-extra-implicit",
//   "-Xfatal-warnings",
//   "-deprecation",
//   "-feature",
//   "-unchecked",
//   // "-Ywarn-unused:implicits",
//   // "-Ywarn-unused:imports",
//   // "-Yno-completion",
//   // "-Ywarn-numeric-widen",
//   // "-Ywarn-value-discard"
// )
// ThisBuild / licenses += ("MIT", url("http://opensource.org/licenses/MIT"))
// ThisBuild / isSnapshot := true

lazy val root = project.in(file(".")).settings(
  name := "proto",
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.3" % Test,
  skip in publish := true,
  scalaVersion := "3.0.0-RC1",
  version := zero.git.version(),
// ).dependsOn(macros).aggregate(macros, runtime, benchmark)
).dependsOn(proto3).aggregate(proto3)

lazy val scala3test = project.in(file("scala3test")).settings(
  name := "proto-macros-test",
  libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",
  skip in publish := true,
  scalaVersion := "3.0.0-RC1",
  version := zero.git.version(),
).dependsOn(proto3).aggregate(proto3)

lazy val proto3 = project.in(file("scala3macros"))

lazy val benchmark = project.in(file("benchmark")).settings(
  // libraryDependencies += "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.10.0",
  // libraryDependencies += "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core" % "2.0.1",
  // libraryDependencies += "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % "2.0.1" % Provided,
  // libraryDependencies += "io.suzaku" %% "boopickle" % "1.3.1",
  // resolvers += Resolver.bintrayRepo("evolutiongaming", "maven"),
  // libraryDependencies += "com.evolutiongaming" %% "kryo-macros" % "1.3.0",
  // PB.targets in Compile := Seq(
  //   scalapb.gen() -> (sourceManaged in Compile).value
  // ),
  skip in publish := true,
  scalaVersion := "3.0.0-RC1",
  version := zero.git.version(),
// ).enablePlugins(JmhPlugin).dependsOn(macros)
).dependsOn(proto3)

turbo := true
useCoursier := true
Global / onChangedBuildSource := ReloadOnSourceChanges
