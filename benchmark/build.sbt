lazy val benchmark = project.in(file(".")).settings(
  libraryDependencies += "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.12.1",
  libraryDependencies += "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core" % "2.6.4",
  libraryDependencies += "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % "2.6.4" % "compile-internal",
  libraryDependencies += "io.suzaku" %% "boopickle" % "1.3.3",
  resolvers += Resolver.bintrayRepo("evolutiongaming", "maven"),
  libraryDependencies += "com.evolutiongaming" %% "kryo-macros" % "1.3.0",
  Compile / PB.targets := Seq(
    scalapb.gen() -> (Compile / sourceManaged).value
  ),
  libraryDependencies += "io.github.zero-deps" %% "proto-macros" % "latest.integration",
  scalaVersion := "2.13.5",
  version := zero.git.version(),
).enablePlugins(JmhPlugin)
