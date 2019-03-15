ThisBuild / scalaVersion := "2.12.7"
ThisBuild / scalacOptions ++= Seq(
  "-Ywarn-extra-implicit",
  "-Xfatal-warnings",
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Ywarn-unused:implicits",
  "-Ywarn-unused:imports",
  "-Yno-completion",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard"
)

lazy val root = (project in file(".")).settings(
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"
).dependsOn(macros)

lazy val macros = (project in file("macros")).settings(
  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
).dependsOn(runtime)

lazy val runtime = (project in file("runtime")).settings(
  libraryDependencies += "com.google.protobuf" % "protobuf-java" % "3.6.1",
)

lazy val benchmark = (project in file("benchmark")).settings(
  libraryDependencies += "com.twitter" %% "chill" % "0.9.3",
  libraryDependencies += "org.scodec" %% "core" % "1.10.4",
  libraryDependencies += "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.9.7",
  libraryDependencies += "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core" % "0.36.9" % Compile,
  libraryDependencies += "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % "0.36.9" % Provided,
  PB.targets in Compile := Seq(
    scalapb.gen() -> (sourceManaged in Compile).value
  )
).enablePlugins(JmhPlugin).dependsOn(macros)


  // val jsoniter = List(
  //   "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core" % "0.36.9" % Compile, 
  //   "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % "0.36.9" % Provided
  // )
  // val chill = "com.twitter" %% "chill" % "0.9.3"
  // val pickling = "com.playtech.mws" %% "scala-pickling" % "1.0-2-gb05b7b9"
  // val jackson = "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.9.7"
  // val argonaut = "io.argonaut" %% "argonaut" % "6.2.2"
