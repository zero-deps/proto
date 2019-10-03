resolvers += Resolver.jcenterRepo

addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.3.4")

addSbtPlugin("com.thesamet" % "sbt-protoc" % "0.99.19")
libraryDependencies += "com.thesamet.scalapb" %% "compilerplugin" % "0.9.1"

libraryDependencies += "org.slf4j" % "slf4j-nop" % "latest.integration"
libraryDependencies += "io.github.zero-deps" %% "gs-git" % "1.5.1"
