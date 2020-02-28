package it.unibo.scafi.plugin.component

import it.unibo.scafi.plugin.ScafiCompilerPlatform
import org.scalatest.{BeforeAndAfterEach, FlatSpec, Matchers}

/**
  * struttura comune ai vari test, contiene una parte di codice che definisce una struttura
  * simile a Scafi per poter verificare l'effettivo funzionamento del plugin
  * @param verbose
  */
class PluginTest(verbose : Boolean = false) extends FlatSpec with BeforeAndAfterEach with Matchers{
  protected var compiler : ScafiCompilerPlatform = _
  override def beforeEach(): Unit = {
    compiler = new ScafiCompilerPlatform(verbose)
  }
  protected val commonCode =
    """
     |package it.unibo.scafi.core
     |trait Language {
     |  trait Constructs {
     |    def nbr[A](expr: => A): A = expr
     |    def foldhood[A](init: => A)(aggr: (A, A) => A)(expr: => A): A = expr
     |    def rep[A](init: =>A)(fun: (A) => A): A = init
     |    def aggregate[A](f: => A) : A = f
     |  }
     |}
     |trait RichLanguage extends Language {
     |  trait Lib {
     |    this: Constructs =>
     |      def nbrRange() : Double = nbr(10.0)
     |  }
     |}
     |trait Semantics extends RichLanguage {
     |  trait ProgramSchema extends Constructs with Lib {
     |    type MainResult = Any
     |    def main() : MainResult
     |  }
     |}
     |object core extends Semantics
    """.stripMargin

  protected def writeInMain(mainBody : String): String = {
    s"""
      $commonCode
      import core._
      class Main extends ProgramSchema {
        override def main() : Unit = {
          $mainBody
        }
      }
    """.stripMargin
  }
}
