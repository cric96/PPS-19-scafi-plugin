package it.unibo.scafi.plugin

import it.unibo.scafi.plugin.component.PluginTest
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestCompiler extends PluginTest {
  "The compiler" should "compile correct code" in {
    compiler.compile("object y extends App").hasErrors shouldBe false
    compiler.compile(
      """
        |class Main extends App {
        | val x = 10
        |}
      """.stripMargin).hasErrors shouldBe false
  }

  "The compiler" should "not compiler bad code" in {
    compiler.compile(
      """
        | object Main extends App {
        |   val x : Ciao = 10
        | }
      """.stripMargin).hasErrors shouldBe true
  }
}