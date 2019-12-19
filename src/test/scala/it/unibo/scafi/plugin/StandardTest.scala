package it.unibo.scafi.plugin

import org.scalatest.{BeforeAndAfterEach, FlatSpec, Matchers}

class StandardTest(verbose : Boolean = false) extends FlatSpec with BeforeAndAfterEach with Matchers{
  protected var compiler : ScafiCompilerPlatform = _
  override def beforeEach(): Unit = {
    compiler = new ScafiCompilerPlatform(verbose)
  }
  /*NB! it is the correct way? it is better to depends on scafi-core? it is a good idea using annotations?*/
  protected val commonCode =
    """
     |trait ProgramSchema
     |trait AggregateProgram extends ProgramSchema {
     |  def nbr[A](expr: => A): A = expr
     |  def foldhood[A](init: => A)(aggr: (A, A) => A)(expr: => A): A = expr
     |}
    """.stripMargin

  protected def writeInMain(mainBody : String): String = {
    s"""
      $commonCode
       class Main extends AggregateProgram {
          def main(x : Int) : Unit = {
            $mainBody
          }
        }
    """.stripMargin
  }
}
