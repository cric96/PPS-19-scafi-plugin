package it.unibo.scafi.plugin

import it.unibo.scafi.definition.AggregateFunction
import it.unibo.scafi.definition._
object ScalaCompilerApp extends App {
  import AggregateFunction._

  val function = aggFun("bibo", T,   AggregateFunction.args(block(T), block((T,T) -> L)))
  /*
  val compiler = new ScafiCompilerPlatform(false)

  val report = compiler.compile(
    """
      | object Main extends App {
      |   val x : Int = 10
      | }
    """.stripMargin)
  println(report)*/
  println(function)
}
