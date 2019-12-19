package it.unibo.scafi.plugin

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class RepCheck extends PluginTest {
  import TypeCheckComponent._
  "Scafi plugin" should "raise an error if there is field value in rep" in {
    val report = compiler.compile(writeInMain {
      """
        | rep{nbr{10}}{x => x}
      """.stripMargin
    })
    report.hasErrors shouldBe true
    report.errors.contains(repErrorString) shouldBe true
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