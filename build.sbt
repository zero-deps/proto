lazy val protopurs = project.in(file(".")).settings(
  scalaVersion := scalav
, crossScalaVersions := scalav :: "2.13.5" :: Nil
, scalacOptions ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 13)) => Nil
      case _ => opts
    }
  }
, libraryDependencies ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 13)) => Seq(
        "org.scala-lang" % "scala-reflect" % scalaVersion.value
      , "org.scalatest" %% "scalatest" % "3.1.1" % Test
      )
      case _ => Nil
    }
  }
)

lazy val protoapi = project.in(file("deps/proto/protoapi"))
lazy val macros = project.in(file("deps/proto/macros"))
lazy val ext = project.in(file("deps/ext"))

dependsOn(protoapi, macros % Test, ext)

val scalav = "3.0.0-RC1"

val opts = Seq(
  "-Yexplicit-nulls"
, "-source", "future-migration"
, "-deprecation"
, "-rewrite"
, "release", "11"
)

turbo := true
useCoursier := true
Global / onChangedBuildSource := ReloadOnSourceChanges
