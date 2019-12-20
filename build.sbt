name := "scafi-plugin-sbt"

version := "0.1"

scalaVersion := "2.11.8"

autoScalaLibrary := true
scalacOptions += "-target:jvm-1.8"

lazy val fixJavaClasspath = taskKey[String]("fix the Java classpath during tests")
fixJavaClasspath := {
  val cp = (fullClasspath in Test).value.map(x => {
    x.data.getAbsolutePath
  }).mkString(":")
  System.setProperty("java.class.path", cp)
}

test in Test := (test in Test).dependsOn(fixJavaClasspath).value
libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-library" % scalaVersion.value,
  "org.scala-lang" % "scala-compiler" % scalaVersion.value,
  "org.scalatest" % "scalatest_2.11" % "3.0.0" % Test,
  "junit" % "junit" % "4.12" % Test
)