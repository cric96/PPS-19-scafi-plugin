package it.unibo.scafi.plugin

import it.unibo.scafi.definition.{AggregateFunction, F, L, T}
import it.unibo.scafi.plugin.TypeCheckComponent.aggregateTypeError
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class DiscoverTest extends PluginTest {
  import AggregateFunction._
  val otherDef = aggFun("myDef", F, args(block(L,T,T)))
  val nested = aggFun("nested", F, args(block(L)))
  "Scafi plugin" should "found new aggregate function" in {
    val report = compiler.compile(writeInMain(
      """
        | def myDef[A](a : => A, b : Int, c : Int) : A = nbr(a)
        |""".stripMargin))
    report.info.contains(DiscoverComponent.resolveAggDefinition(otherDef)) shouldBe true
  }

  "Scafi plugin " should "resolve nested aggregate function" in {
    val report = compiler.compile(writeInMain(
      """
        | def nested[A](a : => A) : A = myDef(a, 10, 20)
        | def myDef[A](a : => A, b : Int, c : Int) : A = nbr(a)
        |""".stripMargin))
    report.info.contains(DiscoverComponent.resolveAggDefinition(nested)) shouldBe true
  }

  "Scafi plugin" should "type check new aggregate function" in {
    val report = compiler.compile(writeInMain(
      """
        | def myDef[A](a : => A, b : Int, c : Int) : A = nbr(a)
        | myDef(nbr(10), 10, 30)
        |""".stripMargin))
    report.hasErrors shouldBe true
    report.errors.contains(aggregateTypeError(otherDef, L, F)) shouldBe true
  }
}
