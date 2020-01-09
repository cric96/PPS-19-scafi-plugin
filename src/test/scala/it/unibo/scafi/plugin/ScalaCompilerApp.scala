package it.unibo.scafi.plugin

object ScalaCompilerApp extends App {
  val compiler = new ScafiCompilerPlatform(false)

  val report = compiler.compile(
    """
      | object Main extends App {
      |   val x : Int = 10
      | }
    """.stripMargin)
  println(report)
}
