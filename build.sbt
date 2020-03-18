name := "scafi-plugin-sbt"

organization := "it.unibo.pps"

scalaVersion := "2.11.8"

scalacOptions += "-target:jvm-1.8"
val env = System.getenv()

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-compiler" % scalaVersion.value,
  "org.scalatest" % "scalatest_2.11" % "3.0.0" % Test,
  "junit" % "junit" % "4.12" % Test
)
//not really important, used to test github packages
def githubTokenFromEnv = if(env.containsKey("GITHUB_TOKEN")) {
  println("[info] token found..")
  Some(TokenSource.Environment("GITHUB_TOKEN"))
} else {
  None
}

ThisBuild / githubOwner := "cric96"
ThisBuild / githubRepository := "PPS-19-scafi-plugin"
ThisBuild / githubTokenSource := githubTokenFromEnv
ThisBuild / githubUser := env.getOrDefault("GITHUB_USER", githubOwner.value)