lazy val macros = project.in(file(".")).settings(
  name := "proto-macros",
  scalaVersion := "3.0.0-RC1",
  version := zero.git.version(),
).dependsOn(api)

lazy val api = project.in(file("../api")).settings(
  name := "proto-api",
  libraryDependencies += "com.google.protobuf" % "protobuf-java" % "3.11.0",
  scalaVersion := "3.0.0-RC1",
  version := zero.git.version(),
)
