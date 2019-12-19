package it.unibo.scafi.plugin

import it.unibo.scafi.plugin.TypeCheckComponent._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class FoldhoodCheck extends PluginTest {
  "Scafi plugin" should "raise an error if there is field value in foldhood" in {
    val report = compiler.compile(writeInMain {
      """
        | foldhood{nbr{10}}{(x,y) => x}{10}
      """.stripMargin
    })
    report.hasErrors shouldBe true
    report.errors.contains(foldHoodErrorString) shouldBe true
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
