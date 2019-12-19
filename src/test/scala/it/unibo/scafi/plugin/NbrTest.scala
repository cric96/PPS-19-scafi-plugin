package it.unibo.scafi.plugin

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class NbrTest extends StandardTest(true) {
  import TypeCheckComponent._
  "the plugin" should "raise an error if there are nested nbr" in {
    val nestedNbr = compiler.compile(
      writeInMain(
        """
          |val x : Int = 2
          |nbr{nbr{10}}
        """.stripMargin
    ).stripMargin)
    nestedNbr.hasErrors shouldBe true
    nestedNbr.errors.contains(nbrNestedErrorString) shouldBe true

    val hideNestedNbr = compiler.compile(
      writeInMain(
        """
          |val x : Int = 2
          |
          |nbr{
          | val y : Int = 10
          | if(y > 10) {
          |   y + 1
          | } else {
          |   nbr(10)
          | }
          |}
        """.stripMargin
    ).stripMargin)
    hideNestedNbr.hasErrors shouldBe true
    hideNestedNbr.errors.contains(nbrNestedErrorString) shouldBe true
  }

  "the plugin" should "allow normal usage of nbr" in {
    val standardNbr = compiler.compile(
      writeInMain(
        """
          |nbr{10}
        """.stripMargin
    ).stripMargin)
    standardNbr.hasErrors shouldBe false
  }
}
