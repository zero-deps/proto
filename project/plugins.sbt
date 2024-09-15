addSbtPlugin("io.github.zero-deps" % "sbt-git" % "2.5.3.gd2541c1")

/* cross-compilation */
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.3.2")
addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % "1.3.2")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.16.0")
addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.5.5")
/* cross-compilation */

/* benchmark */
addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.4.7")
// addSbtPlugin("com.thesamet" % "sbt-protoc" % "1.0.4")
// libraryDependencies += "com.thesamet.scalapb" %% "compilerplugin" % "0.11.3"
/* benchmark */

/* publishing */
addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "3.11.3")
addSbtPlugin("com.github.sbt" % "sbt-pgp" % "2.2.1")
/* publishing */
