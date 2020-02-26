package it.unibo.scafi.plugin

import it.unibo.scafi.definition.{AggregateFunction, AggregateType}

import scala.tools.nsc.Global
import scala.tools.nsc.plugins.PluginComponent
/**
  * abstract plugin component, define general structure of scafi plugin components,
  * has some utility function that could be used in the child components.
  */
abstract class AbstractComponent(protected val context : ComponentContext, protected val descriptor: ComponentDescriptor) extends PluginComponent {
  override val global: Global = context.global
  import global._
  private var componentEnabled = true

  override val phaseName: String = descriptor.name

  override val runsAfter: List[String] = descriptor.runsAfter

  override val runsBefore: List[String] = descriptor.runsBefore

  def disable() : Unit = componentEnabled = false

  private var errorsEnabled = true

  def error(reason : String, pos : Position) : Unit = if(errorsEnabled) {
    globalError(pos, reason)
  } else {
    warning(pos, reason)
  }

  def error(reason : String) : Unit = if(errorsEnabled) {
    globalError(reason)
  } else {
    warning(reason)
  }

  override def enabled: Boolean = componentEnabled
  //by default, component doesn't make anything.
  def processOption(optionName : String, value : String) : Unit = (optionName, value) match {
    case ("error", "disable") => this.errorsEnabled = false
    case ("all", "disable") => this.disable()
    case (`phaseName`, "disable") => this.disable()
    case _ =>
  }
  protected def hasSameName(symbol : Symbol, name : String) : Boolean = symbol.fullName == name
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

    symbol.map(_.selfType.baseClasses)
      .map(classNames => classNames.exists(hasSameName(_, typeName)))
      .exists(p => p)
  }

  protected def search(root : Tree)(extractor : Tree => Option[Tree]) : List[Tree] = extractor(root) match {
    case None => root.children.flatMap(search(_)(extractor))
    case Some(tree) => List(tree)
  }

  protected def searchWithCondition(root : Tree)(condition : Tree => Boolean) : List[Tree] = if(condition(root)) {
    List(root)
  } else {
    root.children.flatMap(searchWithCondition(_)(condition))
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
  //TODO rethink a better solution for uncurry function definition
  protected def uncurry(apply : Apply, uncurryTimes : Int): Apply = (uncurryTimes,apply) match {
    case (0,_) => apply
    case (n, Apply(fun : Apply, _)) => uncurry(fun, n - 1)
    case _ => apply
  }

  protected def expressionsSequence(tree : Tree) : Seq[Tree] = tree match {
    case Block(_, _) => tree.children
    case _ => List(tree)
  }
}

/**
  * the context of the scafi plugin component
  * @param global: the context of compilation
  * @param aggregateFunctions: the set of aggregate function to consider during this compilation.
  */
class ComponentContext(val global : Global,
                       val aggregateProgram : String,
                       val constructs : String,
                       private val aggregateFunctions : Map[String, AggregateFunction]
                      ) {
  private var aggArgMap : Map[Global#Symbol, AggregateType] = Map.empty
  private var aggSymbolMap : Map[Global#Symbol, AggregateFunction] = Map.empty

  def addAggregateFunction(symbol : Global#Symbol, aggDef : AggregateFunction): Unit = aggSymbolMap += symbol -> aggDef

  def addArgumentType(symbol : Global#Symbol, aggType : AggregateType): Unit = aggArgMap += symbol -> aggType

  def extractAggFunctionFromName(name : String) : Option[AggregateFunction] = aggregateFunctions.get(name)

  def extractArgType(arg : Global#Symbol) : Option[AggregateType] = aggArgMap.get(arg)

  def extractAggFunctionFromTree(tree : Global#Tree): Option[AggregateFunction] = tree.symbol match {
    case null => None
    case sym => aggregateFunctions.get(sym.fullName).orElse(aggSymbolMap.get(sym))
  }
}