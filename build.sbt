lazy val proto = project.in(file(".")).settings(
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.5" % Test,
  libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % Test,
  publish / skip := true,
  scalaVersion := "3.0.0-RC1",
  version := zero.git.version(),
).dependsOn(macros).aggregate(macros)

lazy val macros = project.in(file("macros"))

turbo := true
useCoursier := true
Global / onChangedBuildSource := ReloadOnSourceChanges
