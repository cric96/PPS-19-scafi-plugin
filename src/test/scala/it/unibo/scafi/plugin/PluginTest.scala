package it.unibo.scafi.plugin

import org.scalatest.{BeforeAndAfterEach, FlatSpec, Matchers}

class PluginTest(verbose : Boolean = false) extends FlatSpec with BeforeAndAfterEach with Matchers{
  protected var compiler : ScafiCompilerPlatform = _
  override def beforeEach(): Unit = {
    compiler = new ScafiCompilerPlatform(verbose)
  }
  /*NB! it is the correct way? it is better to depends on scafi-core? it is a good idea using annotations?*/
  protected val commonCode =
    """
     |trait ProgramSchema {
     |  def main() : Unit
     |}
     |trait AggregateProgram extends ProgramSchema {
     |  def nbr[A](expr: => A): A = expr
     |  def foldhood[A](init: => A)(aggr: (A, A) => A)(expr: => A): A = expr
     |  def rep[A](init: =>A)(fun: (A) => A): A = init
     |  def aggregate[A](f: => A) : A = f
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
