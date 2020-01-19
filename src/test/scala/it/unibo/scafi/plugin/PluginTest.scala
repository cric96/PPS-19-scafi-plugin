package it.unibo.scafi.plugin

import org.scalatest.{BeforeAndAfterEach, FlatSpec, Matchers}

class PluginTest(verbose : Boolean = false) extends FlatSpec with BeforeAndAfterEach with Matchers{
  protected var compiler : ScafiCompilerPlatform = _
  override def beforeEach(): Unit = {
    compiler = new ScafiCompilerPlatform(verbose)
  }
  protected val commonCode =
    """
     |trait Constructs {
     |  def nbr[A](expr: => A): A
     |  def foldhood[A](init: => A)(aggr: (A, A) => A)(expr: => A): A
     |  def rep[A](init: =>A)(fun: (A) => A): A
     |  def aggregate[A](f: => A) : A
     |}
     |trait Lib {
     |  this: Constructs =>
     |  def nbrRange() : Double = nbr(10.0)
     |}
     |trait ProgramSchema {
     |  def main() : Unit
     |}
     |trait AggregateProgram extends ProgramSchema with Constructs{
     |  override def nbr[A](expr: => A): A = expr
     |  override def foldhood[A](init: => A)(aggr: (A, A) => A)(expr: => A): A = expr
     |  override def rep[A](init: =>A)(fun: (A) => A): A = init
     |  override def aggregate[A](f: => A) : A = f
     |}
    """.stripMargin

  protected def writeInMain(mainBody : String): String = {
    s"""
      $commonCode
       class Main extends AggregateProgram {
          override def main() : Unit = {
            $mainBody
          }
        }
    """.stripMargin
  }
}
