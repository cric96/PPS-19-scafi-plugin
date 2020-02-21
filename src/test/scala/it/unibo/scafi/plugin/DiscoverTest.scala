package it.unibo.scafi.plugin

import it.unibo.scafi.definition.{AggregateFunction, F, L, T}
import it.unibo.scafi.plugin.TypeCheckComponent.aggregateTypeError
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class DiscoverTest extends PluginTest {
  import AggregateFunction._
  val otherDef = aggFun("it.unibo.scafi.core.Main.myDef", F, args(block(L,T,T)))
  val recursiveDef = aggFun("it.unibo.scafi.core.Main.myDef", T, args(block(T,T,T)))
  val nested = aggFun("it.unibo.scafi.core.Main.nested", F, args(block(L)))
  val otherFoldhood = aggFun("it.unibo.scafi.core.Main.foldhood", T, args())
  "Scafi plugin" should "found new aggregate function" in {
    val report = compiler.compile(writeInMain(
      """
        | def myDef[A](a : => A, b : Int, c : Int) : A = nbr(a)
        |""".stripMargin))
    println(report.info)
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
  "Scafi plugin " should "resolve recursive aggregate function" in {
    val report = compiler.compile(writeInMain(
      """
        | def myDef[A](a : => A, b : Int, c : Int) : A = {
        |   myDef(a, b, c)
        | }
        |""".stripMargin))
    report.info.contains(DiscoverComponent.resolveAggDefinition(recursiveDef)) shouldBe true
  }
  "Scafi plugin " should "resolve multiple name definitions aggregate function" in {
    val report = compiler.compile(writeInMain(
      """
        | def foldhood() : Int = {10}
        | val x = foldhood()
        |""".stripMargin))
    report.info.contains(DiscoverComponent.resolveAggDefinition(otherFoldhood)) shouldBe true
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
  "Scafi plugin" should "resolve generic function" in {
    val report = compiler.compile(writeInMain(
      """
        |trait Ordered[A] {
        |  def compare(a: A, b: A): Int
        |  def same(a: A, b: A): Boolean = compare(a, b) == 0
        |  def min(a: A, b: A): A = if (compare(a, b) <= 0) a else b
        |  def max(a: A, b: A): A = if (compare(a, b) > 0) a else b
        |}
        |def G[V: Ordered](source: Boolean, field: V, acc: V => V, metric: => Double): V = {
        | field
        |}
        |class MyInt extends Ordered[Int] {
        |  def compare(a : Int, b : Int) : Int = a - b
        |}
        |implicit val of_i = new MyInt()
        |def other(x : Int) : Int = {
        | G[Int](true, x, a => a, 10)
        |}
        |""".stripMargin))
    println(report.errors)
  }
}
