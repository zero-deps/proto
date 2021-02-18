lazy val proto3 = project.in(file(".")).settings(
  name := "proto-macros",
  scalaVersion := "3.0.0-RC1",
  version := zero.git.version(),
  // libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
).dependsOn(runtime)

lazy val runtime = project.in(file("../runtime")).settings(
  name := "proto-runtime",
  libraryDependencies += "com.google.protobuf" % "protobuf-java" % "3.11.0",
  scalaVersion := "3.0.0-RC1",
  version := zero.git.version(),
)
