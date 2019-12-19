package it.unibo.scafi.plugin

import scala.tools.nsc.Global
import scala.tools.nsc.plugins.PluginComponent
/**
  * abstract plugin component, define general structure of scafi plugin components,
  * has some utility function that could be used in the child components.
  */
abstract class CommonComponent(protected val context : ComponentContext) extends PluginComponent {
  override val global: Global = context.global
  import global._

  protected def hasSameName(symbol : Symbol, name : String) : Boolean = symbol.name.containsName(name)
  /**
    * return the symbol from tree if certain condition are satisfied.
    * @param tree : object where check the condition passed
    * @param condition : the condition used to verify the symbol presence in the tree
    * @return Some(tree.symbol) is the condition are satisfied, None otherwise
    */
  protected def extractSymbolIf(tree : Tree, condition : Tree => Boolean) : Option[Symbol] = Some(tree).filter(condition).map(_.symbol)

  /**
    * verify if the root tree, is a aggregate program, it could be:
    *   - a class definition that extends the trait name defined in scafi names.
    *   - an object that extends the trait name defined in scafi names
    * @param tree: the root where check the conditions
    */
  protected def isAggregateProgram(tree : Tree) : Boolean = {
    def containsAggregateName(name : Symbol) : Boolean = hasSameName(name, context.names.aggregateProgram)

    val symbol = extractSymbolIf(tree, {
      case ClassDef(_,_,_,_) => true
      case ModuleDef(_,_,_) => true
      case _ => false
    })

    symbol.map(_.baseClasses)
      .map(classNames => classNames.filter(containsAggregateName))
      .nonEmpty
  }

  /**
    * extract all aggregate programs from a tree
    * @param tree: the structure where extract aggregate programs
    * @return list of aggregate programs if are defined, List.empty otherwise
    */
  protected def extractAggregatePrograms(tree : Tree) : List[Tree] = {
    if(isAggregateProgram(tree)) {
      List(tree)
    } else {
      tree.children.flatMap(extractAggregatePrograms)
    }
  }
}
/**
  * the context of the scafi plugin component
  * @param global: the context of compilation
  * @param names: the main name used in the scafi programs (constructor, class, function)
  */
case class ComponentContext(global : Global, names : ScafiNames)