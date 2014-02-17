import AssemblyKeys._   // put this at the top of the file

assemblySettings

mainClass in assembly := Some("dke.tradesim.Runner")

name := "tradesim"

mainClass in (Compile, run) := Some("dke.tradesim.Runner")

version := "1.0"

scalaVersion := "2.10.3"

//javacOptions ++= Seq("-Xlint:unchecked")  // these unchecked warnings are caused by the thrift-generated java code

scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked",
  "-Xelide-below", "INFO",
  "-feature",
  "-language", "implicitConversions"
)

resolvers ++= Seq(
  // other resolvers here
  // if you want to use snapshot builds (currently 0.6-SNAPSHOT), use this.
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
)

libraryDependencies ++= Seq(
  "joda-time" % "joda-time" % "2.3",
  "org.joda" % "joda-convert" % "1.2",
  "com.github.nscala-time" %% "nscala-time" % "0.6.0",
  "javax.transaction" % "jta" % "1.1",
  "net.sf.ehcache" % "ehcache" % "2.7.4",
  "com.typesafe.slick" %% "slick" % "2.0.0",
  "org.slf4j" % "slf4j-nop" % "1.6.4",
  "postgresql" % "postgresql" % "9.1-901.jdbc4",
  "org.rogach" %% "scallop" % "0.9.2",
  "org.json4s" %% "json4s-jackson" % "3.2.2",
  "com.lambdaworks" % "jacks_2.10" % "2.2.0",
  "net.sandrogrzicic" %% "scalabuff-runtime" % "1.3.6",
  "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test",
  "org.scala-lang" % "scala-compiler" % "2.10.2",
  "org.scala-lang" % "jline" % "2.10.2",
  "org.spire-math" %% "spire" % "0.6.0",
  "org.scalanlp" % "breeze_2.10" % "0.6",
  "org.scalanlp" % "nak" % "1.2.0"
)
