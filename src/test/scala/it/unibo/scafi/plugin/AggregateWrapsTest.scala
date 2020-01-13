package it.unibo.scafi.plugin

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

//TODO improve test
@RunWith(classOf[JUnitRunner])
class AggregateWrapsTest extends PluginTest {
  "Scafi plugin" should "wraps lambda using aggregate" in {
    val (singleCode, singleReport) = compiler.transform(writeInMain {
      """
        | foldhood{15}{(x,y) => {x;y}}{nbr(10)}
      """.stripMargin
    })
    singleReport.hasErrors shouldBe false
    singleCode.contains(
      """|aggregate({
         |      x;
         |      y
         |    }))""".stripMargin) shouldBe true
    val (multiCode, multiReport) = compiler.transform(writeInMain {
      """
        | foldhood{15}{(x,y) => x}{nbr(10)}
        | foldhood{15}{(h,y) => y}{nbr(10)}
      """.stripMargin
    })
    multiReport.hasErrors shouldBe false
    multiCode contains "(x, y) => aggregate(x)" shouldBe true
    multiCode contains "(h, y) => aggregate(y)" shouldBe true
  }
  "Scafi plugin" should "doesn't wrap lambda with already aggregate wrapping" in {
    val (code, report) = compiler.transform(writeInMain {
      """
        | foldhood{15}{(x) => aggregate(x)}{nbr(10)}
      """.stripMargin
    })
    report.hasErrors shouldBe false
    code contains "(x) => aggregate(x)" shouldBe true
  }
}
