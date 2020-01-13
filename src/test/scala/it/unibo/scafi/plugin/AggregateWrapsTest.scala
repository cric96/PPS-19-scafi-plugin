package it.unibo.scafi.plugin

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

//TODO improve test
@RunWith(classOf[JUnitRunner])
class AggregateWrapsTest extends PluginTest {
  "Scafi plugin" should "wraps lambda using aggregate" in {
    val report = compiler.compile(writeInMain {
      """
        | foldhood{15}{(x,y) => {x;y}}{nbr(10)}
      """.stripMargin
    })
    println(report.errors)
    report.hasErrors shouldBe false
    println(report.code)
  }
}
