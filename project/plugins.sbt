resolvers += Resolver.githubPackages("zero-deps")

addSbtPlugin("io.github.zero-deps" % "sbt-git" % "latest.integration")
addSbtPlugin("ch.epfl.lamp" % "sbt-dotty" % "latest.integration")

// addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.3.7")

// addSbtPlugin("com.thesamet" % "sbt-protoc" % "0.99.25")
// libraryDependencies += "com.thesamet.scalapb" %% "compilerplugin" % "0.9.3"
