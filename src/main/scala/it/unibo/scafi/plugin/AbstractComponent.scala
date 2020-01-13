package it.unibo.scafi.plugin

import it.unibo.scafi.definition.AggregateFunction

import scala.tools.nsc.Global
import scala.tools.nsc.plugins.PluginComponent
/**
  * abstract plugin component, define general structure of scafi plugin components,
  * has some utility function that could be used in the child components.
  */
abstract class AbstractComponent(protected val context : ComponentContext, protected val descriptor: ComponentDescriptor) extends PluginComponent {
  override val global: Global = context.global
  import global._

  override val phaseName: String = descriptor.name

  override val runsAfter: List[String] = descriptor.runsAfter

  override val runsBefore: List[String] = descriptor.runsBefore
  protected def hasSameName(symbol : Symbol, name : String) : Boolean = {
    //TODO! it is the right way to check name ??
    symbol.nameString == name
  }
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
    def containsAggregateName(name : Symbol) : Boolean = hasSameName(name, context.main)
    val symbol = extractSymbolIf(tree, {
      case ClassDef(_,_,_,_) => true
      case ModuleDef(_,_,_) => true
      case _ => false
    })

    symbol.map(_.baseClasses)
      .map(classNames => classNames.filter(containsAggregateName))
      .nonEmpty
  }

  def extractAggregateMain(tree : Tree) : Option[DefDef] = {
    tree match {
      case defMain : DefDef if defMain.name.endsWith("main") =>
        defMain.mods match {
          case Modifiers(Flag.OVERRIDE, _, _) => Some(defMain)
          case _ => None
        }
      case _ => None
    }
  }

  protected def extractAggregateFunction(tree : Tree) : Option[AggregateFunction] = tree.symbol match {
    case null => None
    case _ => context.aggregateFunctions.get(tree.symbol.nameString)
  }

  protected def extractAggregateProgram(tree : Tree) : Option[Tree] = Some(tree).filter(isAggregateProgram)
  /**
    * search all aggregate programs from a tree
    * @param tree: the structure where extract aggregate programs
    * @return list of aggregate programs if are defined, List.empty otherwise
    */
  protected def searchAggregatePrograms(tree : Tree) : List[Tree] = extractAggregateProgram(tree) match {
    case None => tree.children.flatMap(searchAggregatePrograms)
    case Some(tree) => List(tree)
  }
}

/**
  * the context of the scafi plugin component
  * @param global: the context of compilation
  * @param aggregateFunctions: the set of aggregate function to consider during this compilation.
  */
case class ComponentContext(global : Global, main : String, aggregateFunctions : Map[String, AggregateFunction])