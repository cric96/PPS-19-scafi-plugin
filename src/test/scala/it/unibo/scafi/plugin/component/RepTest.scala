package it.unibo.scafi.plugin.component

import it.unibo.scafi.definition.AggregateFunction.{aggFun, args, block}
import it.unibo.scafi.definition.{F, L, T}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class RepTest extends PluginTest {
  import TypeCheckComponent._
  val repSig = aggFun("it.unibo.scafi.core.Language.Constructs.rep", returns = L, args(block(L), block(T)))

  "Scafi plugin" should "raise an error if there is field value in rep" in {
    val report = compiler.compile(writeInMain {
      """
        | rep{nbr{10}}{x => x}
      """.stripMargin
    })
    report.hasErrors shouldBe true
    report.errors.contains(aggregateTypeError(repSig, L, F)) shouldBe true
  }

  "Scafi plugin" should "work as usual with rep" in {
    val report = compiler.compile(writeInMain {
      """
        | rep{10}{x => x + 10}
      """.stripMargin
    })
    report.hasErrors shouldBe false
  }
}
