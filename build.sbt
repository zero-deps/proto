lazy val root = project.in(file(".")).settings(
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
).dependsOn(protoscala).aggregate(protoscala, protosyntax, benchmark, protopurs, prototex)

lazy val protoscala = project.in(file("protoscala"))
lazy val protosyntax = project.in(file("syntax"))
lazy val benchmark = project.in(file("benchmark"))
lazy val protopurs = project.in(file("purs"))
lazy val prototex = project.in(file("tex"))
lazy val protoops = project.in(file("ops"))

turbo := true
useCoursier := true
Global / onChangedBuildSource := ReloadOnSourceChanges
