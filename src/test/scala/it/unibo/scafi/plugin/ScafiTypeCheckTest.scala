package it.unibo.scafi.plugin

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfterEach, FlatSpec, Matchers}

@RunWith(classOf[JUnitRunner])
class ScafiTypeCheckTest extends FlatSpec with BeforeAndAfterEach with Matchers {
  var compiler : ScafiCompilerPlatform = _
  import TypeCheckComponent._

  override def beforeEach(): Unit = {
    compiler = new ScafiCompilerPlatform(false)
  }

  val commonCode =
    """
      |trait ProgramSchema
      |trait AggregateProgram extends ProgramSchema
    """.stripMargin

  def writeInMain(mainBody : String) = {
    s"""
      $commonCode
       class Main extends ProgramSchema {
          def main(x : Int) : Unit = {
            $mainBody
          }
        }
    """.stripMargin
  }

  "Scafi pluging" should "check if presence in main" in {
    val reportSingleIf = compiler.compile(writeInMain {
       """
         |val x = 10
         |val y = 20
         |if(x == 10) y else x
       """.stripMargin
      }
    )
    reportSingleIf.hasErrors shouldBe false
    reportSingleIf.warnings.contains(ifInfoString) shouldBe true

    val reportMultipleIf = compiler.compile(writeInMain {
      """
        |val x = 10
        |val y = 20
        |if(x == 10) y else x
        |if(y > 10) y else x
      """.stripMargin
    })

    reportMultipleIf.hasErrors shouldBe false
    reportMultipleIf.warnings.count(_ == ifInfoString) shouldBe 2
  }

  "Scafi pluging" should "check if presence only aggregate main" in {
    val report = compiler.compile{
      """
        | class AClass {
        |   def main() : Boolean = {
        |     if(true) true else false
        |   }
        | }
      """.stripMargin
    }
    report.hasErrors shouldBe false
    report.warnings.contains(ifInfoString) shouldBe false
  }
}
