package it.unibo.scafi.plugin

import it.unibo.scafi.definition.{AggregateFunction, F, L, T}

import scala.tools.nsc.Global
import scala.tools.nsc.plugins.Plugin

/**
  * This plugin support compile-time checking in the aggregate programs.
  * It can change the program structure too.
  * The plugin has two different components:
  *   - type check component: do all compile time checking in the aggregate program,
  *   - discover component: find all aggregate function definitions in current compilation,
  *   - transform component: find all thing that the programs need to transform to satisfy certain properties,
  *       for example(??): find all function in the aggregate program and wraps into aggregate constructor
  */
class ScafiDSLPlugin(val global: Global) extends Plugin {
  //global include all compile time information.
  implicit val g: Global = global
  //the term used to check and find the properties of aggregate program
  import AggregateFunction._
  private val baseClass = "it.unibo.scafi.core.Language.Constructs"
  private val baseClassImpl = "it.unibo.scafi.core.Semantics.ConstructsSemantics"
  //TODO make parametrizable with file.
  private val coreFunction = AggregateFunction.toMap(
    aggFun(s"$baseClass.nbr", returns = F, args(block(L))),
    aggFun(s"$baseClass.foldhood", returns = L, args(block(L), block((T,T) -> T), block(F))),
    aggFun(s"$baseClass.rep", returns = L, args(block(L), block(T))),
    //because symbol is different from implementation!
    aggFun(s"$baseClassImpl.nbr", returns = F, args(block(L))),
    aggFun(s"$baseClassImpl.foldhood", returns = L, args(block(L), block((T,T) -> T), block(F))),
    aggFun(s"$baseClassImpl.rep", returns = L, args(block(L), block(T)))
  )
  private val baseAggregateProgram = "it.unibo.scafi.core.Semantics.ProgramSchema"
  //the context used in all plugin components.
  implicit val componentContext : ComponentContext = new ComponentContext(g, baseAggregateProgram, baseClass, coreFunction)
  override val name: String = "scafi"
  override val components: List[AbstractComponent] = List(
    TransformComponent(),
    TypeCheckComponent(),
    DiscoverComponent()
  )
  override val description: String = "check the type correctness of aggregate program and transform the program (if needed)"

  override def processOptions(options: List[String], error: String => Unit): Unit = {
    for (option <- options) {
      val splitted = option.split('=')
      if(splitted.length != 2) {
        throw new IllegalArgumentException("Invalid option")
      }
      components.foreach(_.processOption(splitted(0), splitted(1)))
    }
  }
}
