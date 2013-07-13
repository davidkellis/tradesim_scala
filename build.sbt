name := "tradesim"

version := "1.0"

scalaVersion := "2.10.1"

scalacOptions ++= Seq(
  "-deprecation",
  "-Xelide-below", "INFO"
)

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies ++= Seq(
  "joda-time" % "joda-time" % "2.2",
  "org.joda" % "joda-convert" % "1.2",
  "javax.transaction" % "jta" % "1.1",
  "net.sf.ehcache" % "ehcache" % "2.7.0",
  "com.typesafe.slick" % "slick_2.10" % "1.0.0",
  "org.mongodb" %% "casbah" % "2.6.0",
  "org.slf4j" % "slf4j-nop" % "1.6.4",
  "postgresql" % "postgresql" % "9.1-901.jdbc4",
  "org.rogach" %% "scallop" % "0.9.2",
  "org.json4s" %% "json4s-jackson" % "3.2.2",
  "com.lambdaworks" % "jacks_2.10" % "2.2.0"
)
