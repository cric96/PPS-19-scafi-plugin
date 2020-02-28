name := "scafi-plugin-sbt"

organization := "it.unibo.pps"

scalaVersion := "2.11.8"

scalacOptions += "-target:jvm-1.8"
val env = System.getenv()
//resolve sbt test on remote repository
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