package it.unibo.scafi.plugin
import it.unibo.scafi.definition.AggregateFunction._
import it.unibo.scafi.definition._
import it.unibo.scafi.plugin.TypeCheckComponent._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class FoldhoodTest extends PluginTest {
  val foldhoodSig = aggFun("foldhood", L, args(block(L), block(T), block(F)))
  "Scafi plugin" should "raise an error if foldhood call isn't correct" in {
    val localWrongReport = compiler.compile(writeInMain {
      """
        | foldhood{nbr{10}}{(x,y) => x}{nbr(10)}
      """.stripMargin
    })
    localWrongReport.hasErrors shouldBe true
    localWrongReport.errors.contains(aggregateTypeError(foldhoodSig, L, F)) shouldBe true

    val fieldWrongReport = compiler.compile(writeInMain {
      """
        | foldhood{10}{(x,y) => x}{10}
        |""".stripMargin
    })
    fieldWrongReport.hasErrors shouldBe true
    fieldWrongReport.errors.contains(aggregateTypeError(foldhoodSig, F, L)) shouldBe true
  }
  "Scafi plugin" should "work as usual with foldhood" in {
    val report = compiler.compile(writeInMain {
      """
        | foldhood{15}{(x,y) => x}{nbr(10)}
      """.stripMargin
    })
    report.hasErrors shouldBe false
  }
}
