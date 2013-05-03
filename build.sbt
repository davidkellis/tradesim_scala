name := "tradesim"

version := "1.0"

scalaVersion := "2.10.0"

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies ++= Seq(
  "joda-time" % "joda-time" % "2.2",
  "org.joda" % "joda-convert" % "1.2",
  "net.sf.ehcache" % "ehcache" % "2.7.0",
  "com.typesafe.slick" %% "slick" % "1.0.0",
  "org.slf4j" % "slf4j-nop" % "1.6.4",
  "postgresql" % "postgresql" % "9.1-901.jdbc4",
  "com.github.scopt" %% "scopt" % "2.1.0"
)
