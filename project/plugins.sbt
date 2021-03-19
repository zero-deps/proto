addSbtPlugin("io.github.zero-deps" % "sbt-git" % "2.5.3.gd2541c1")

/* benchmark */
addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.4.0")
addSbtPlugin("com.thesamet" % "sbt-protoc" % "1.0.0")
libraryDependencies += "com.thesamet.scalapb" %% "compilerplugin" % "0.10.10"
/* benchmark */

/* publishing */
addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "latest.integration")
addSbtPlugin("com.github.sbt" % "sbt-pgp" % "latest.integration")
/* publishing */
