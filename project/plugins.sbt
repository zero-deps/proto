resolvers += "GitHub Package Registry (zero-deps)" at "https://maven.pkg.github.com/zero-deps/_"

addSbtPlugin("io.github.zero-deps" % "sbt-git" % "latest.integration")

/* publishing */
addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "latest.integration")
addSbtPlugin("com.github.sbt" % "sbt-pgp" % "latest.integration")
/* publishing */
