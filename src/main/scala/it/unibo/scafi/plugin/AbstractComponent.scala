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

  protected def extendsFromType(tree : Tree, typeName : String) : Boolean = {
    val symbol = extractSymbolIf(tree, {
      case ClassDef(_,_,_,_) => true
      case ModuleDef(_,_,_) => true
      case _ => false
    })

    symbol.map(_.baseClasses)
      .map(classNames => classNames.filter(hasSameName(_, typeName)))
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

  protected def uncurry(apply : Apply, uncurryTimes : Int): Apply = (uncurryTimes,apply) match {
    case (0,_) => apply
    case (n, Apply(fun : Apply, _)) => uncurry(fun, n - 1)
    case _ => apply
  }
}

/**
  * the context of the scafi plugin component
  * @param global: the context of compilation
  * @param aggregateFunctions: the set of aggregate function to consider during this compilation.
  */
case class ComponentContext(global : Global,
                            aggregateProgram : String,
                            constructs : String,
                            aggregateFunctions : Map[String, AggregateFunction])