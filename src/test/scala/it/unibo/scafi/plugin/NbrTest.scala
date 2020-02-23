package it.unibo.scafi.plugin

import it.unibo.scafi.definition.AggregateFunction._
import it.unibo.scafi.definition._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class NbrTest extends PluginTest(false) {
  import TypeCheckComponent._
  val nbrSig = aggFun("it.unibo.scafi.core.Language.Constructs.nbr", F, args(block(L)))

  "Scafi plugin" should "raise an error if there are nested nbr" in {
    val nestedNbr = compiler.compile(
      writeInMain(
        """
          |val x : Int = 2
          |nbr{nbr{10}}
        """.stripMargin
    ).stripMargin)
    nestedNbr.hasErrors shouldBe true
    nestedNbr.errors.contains(aggregateTypeError(nbrSig, L, F)) shouldBe true
    /*
    //TODO think how to manage nested constructor. if is not accepted inside...
    val hideNestedNbr = compiler.compile(
      writeInMain(
        """
          |val x : Int = 2
          |
          |nbr {
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
    hideNestedNbr.errors.contains(aggregateTypeError(nbrSig, L, F)) shouldBe true
    */
  }

  "Scafi plugin" should "allow normal usage of nbr" in {
    val standardNbr = compiler.compile(
      writeInMain(
        """
          |nbr{10}
        """.stripMargin
    ).stripMargin)
    standardNbr.hasErrors shouldBe false
  }
}
