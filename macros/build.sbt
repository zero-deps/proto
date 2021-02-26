lazy val macros = project.in(file(".")).settings(
  name := "proto-macros",
  scalaVersion := "3.0.0-RC1",
  crossScalaVersions := "3.0.0-RC1" :: "2.13.5" :: Nil,
  resolvers += Resolver.JCenterRepository,
  version := zero.git.version(),
  libraryDependencies ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 13)) =>
        Seq(
          "org.scala-lang" % "scala-reflect" % scalaVersion.value,
        )
      case _ => Nil
    }
  },
).dependsOn(api)

lazy val api = project.in(file("../api")).settings(
  name := "proto-api",
  libraryDependencies += "com.google.protobuf" % "protobuf-java" % "3.15.2",
  scalaVersion := "3.0.0-RC1",
  crossScalaVersions := "3.0.0-RC1" :: "2.13.5" :: Nil,
  resolvers += Resolver.JCenterRepository,
  version := zero.git.version(),
)

