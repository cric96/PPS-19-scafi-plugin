name := "scafi-plugin-sbt"

version := "0.1"

scalaVersion := "2.11.8"

autoScalaLibrary := true
scalacOptions += "-target:jvm-1.8"
//TO REMOVE
resolvers += "Github package" at "https://maven.pkg.github.com/cric96/PPS-19-scafi-plugin"
credentials += Credentials("Github credentials", "maven.pkg.github.com", "cric96", "23c97649de607944bdcf49308ce0a6bd93eff79a")
//resolve sbt test on remote repository
lazy val fixJavaClasspath = taskKey[String]("fix the Java classpath during tests")
fixJavaClasspath := {
  val cp = (fullClasspath in Test).value.map(x => {
    x.data.getAbsolutePath
  }).mkString(":")
  System.setProperty("java.class.path", cp)
}

test in Test := (test in Test).dependsOn(fixJavaClasspath).value
//TO REMOVE
autoCompilerPlugins := true
addCompilerPlugin("it.unibo.pps" % "scafi-plugin-sbt_2.11"% "0.1")
libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-library" % scalaVersion.value,
  "org.scala-lang" % "scala-compiler" % scalaVersion.value,
  "org.scalatest" % "scalatest_2.11" % "3.0.0" % Test,
  "junit" % "junit" % "4.12" % Test
)
//publish settings:
ThisBuild / githubOwner := "cric96"
ThisBuild / githubRepository := "PPS-19-scafi-plugin"
ThisBuild / githubTokenSource := Some(TokenSource.Environment("GITHUB_TOKEN"))
ThisBuild / githubUser := sys.env("GITHUB_USER")