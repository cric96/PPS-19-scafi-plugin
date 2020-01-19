package it.unibo.scafi.plugin

import it.unibo.scafi.definition.{AggregateFunction, AggregateType, ArrowType, F, L, T}

import scala.tools.nsc.Global
import scala.tools.nsc.plugins.{Plugin, PluginComponent}

/**
  * //TODO add discover phases: find all aggregate functions.
  * This plugin support compile-time checking in the aggregate programs.
  * It can change the program structure too.
  * The plugin has two different components:
  *   - type check component: do all compile time checking in the aggregate program
  *   - transform component: find all thing that the programs need to transform to satisfy certain properties,
  *       for example(??): find all function in the aggregate program and wraps into aggregate constructor
  */
class ScafiDSLPlugin(val global: Global) extends Plugin {
  //global include all compile time information.
  implicit val g: Global = global
  //the term used to check and find the properties of aggregate program
  import AggregateFunction._
  private val coreFunction = AggregateFunction.toMap(
    aggFun("nbr", returns = F, args(block(L))),
    aggFun("foldhood", returns = L, args(block(L), block((T,T) -> T), block(F))),
    aggFun("rep", returns = L, args(block(L), block(T)))
  )

  //the context used in all plugin components.
  implicit val componentContext : ComponentContext = ComponentContext(g, "ProgramSchema", "Constructs", coreFunction)
  override val name: String = "scafiplugin"
  override lazy val components: List[PluginComponent] = List(
    TransformComponent(),
    TypeCheckComponent(),
    DiscoverComponent()
  )
  override val description: String = "check the type correctness of aggregate program and transform the program (if needed)"

  override def processOptions(options: List[String], error: String => Unit): Unit = {
    for (option <- options) {
      /*TODO, think if is a good idea to add some params, and understood how*/
    }
  }

}
