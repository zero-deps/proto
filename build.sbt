lazy val proto = project.in(file(".")).settings(
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.5" % Test,
  publish / skip := true,
  scalaVersion := "3.0.0-RC1",
  crossScalaVersions := "3.0.0-RC1" :: "2.13.5" :: Nil,
  resolvers += Resolver.JCenterRepository,
  Test / scalacOptions ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 13)) => Nil
      case _ =>
        Seq(
          "-source", "future-migration"
        , "-deprecation"
        , "-rewrite"
        )
    }
  },
  version := zero.git.version(),
).dependsOn(macros).aggregate(macros, syntax, benchmark, protopurs)

lazy val macros = project.in(file("macros"))
lazy val syntax = project.in(file("syntax"))

lazy val benchmark = project.in(file("benchmark"))

lazy val protopurs = project.in(file("purs"))

turbo := true
useCoursier := true
Global / onChangedBuildSource := ReloadOnSourceChanges
