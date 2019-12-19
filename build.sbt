name := "scafi-plugin-sbt"

version := "0.1"

scalaVersion := "2.11.8"

autoScalaLibrary := true

javaHome := sys.env.get("JAVA_HOME") map file
scalacOptions += "-target:jvm-1.8"
libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-library" % scalaVersion.value,
  "org.scala-lang" % "scala-compiler" % scalaVersion.value,
  "org.scala-lang" % "scala-reflect" % scalaVersion.value % Test,
  "org.scalatest" % "scalatest_2.11" % "3.0.0" % Test,
  "junit" % "junit" % "4.12" % Test
)