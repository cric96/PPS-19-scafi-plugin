package it.unibo.scafi.plugin.common

import scala.tools.nsc.Global
import scala.tools.nsc.plugins.PluginComponent
/**
  * AbstractComponent defines a general structure of scafi plugin components,
  * has some utility function that could be used by its children components.
  * A component could be disabled passing a specific option to the compiler.
  * To disable a component you need to write:
  *   -Pscafi:disable:<componentName>.
  * each component has a component descriptor, that contains the main information to describe a component.
  * The context is shared among all components.
  */
abstract class AbstractComponent(protected val context : ComponentContext, protected val descriptor: ComponentDescriptor) extends PluginComponent {
  override val global: Global = context.global

  import global._

  private var componentEnabled = true

  override val phaseName: String = descriptor.name

  override val runsAfter: List[String] = descriptor.runsAfter

  override val runsBefore: List[String] = descriptor.runsBefore
  //allow to disable a component in the compilation phases
  private def disable() : Unit = componentEnabled = false

  private var errorsEnabled = true

  /**
    * method used to underline aggregate checking error during computation,
    * if errors are disabled, only warnings are showed
    */
  protected def error(reason : String, pos : Position) : Unit = if(errorsEnabled) {
    globalError(pos, reason)
  } else {
    warning(pos, reason)
  }

  protected def error(reason : String) : Unit = if(errorsEnabled) {
    globalError(reason)
  } else {
    warning(reason)
  }

  override def enabled: Boolean = componentEnabled

  /**
    * It process the options received from compiler options.
    * the option supported are:
    *   disable:error => each error becomes a warning
    *   disable:* => no component is enabled
    *   disable:<component_name> => only <component_name> is disabled
    */
  def processOption(optionName : String, value : String) : Unit = {
    import AbstractComponent.ComponentOption._
    (optionName, value) match {
      case (`disableOption`, `errorValue`) => this.errorsEnabled = false
      case (`disableOption`, `allValue`) => this.disable()
      case (`disableOption`, `phaseName`) => this.disable()
      case _ =>
    }
  }
  /**
    * it verifies if the fullName symbol is equal to the passed name
    */
  protected def symbolMatchWithName(symbol : Symbol, name : String) : Boolean = symbol.fullName == name
  /**
    * it returns the symbol got from the tree if some condition are satisfied.
    * @param tree : where to check the condition passed
    * @param condition : it is used to verify the symbol presence in the tree
    * @return Some(tree.symbol) if the condition is satisfied, None otherwise
    */
  protected def extractSymbolIf(tree : Tree, condition : Tree => Boolean) : Option[Symbol] = Some(tree).filter(condition).map(_.symbol)
  /**
    * It verifies if some tree extends from a parent type,
    * checked by typeName which is the parent in term of fullName.
    * in this case, the AST must be a ClassDef or ModuleDef for verifying the
    * typing.
    * @param tree : the tree to checks, must be a ClassDef or ModuleDef. In other
    *             cases the method return false
    * @param typeName : the name of upper type in term of full symbol name
    */
  protected def extendsFromType(tree : Tree, typeName : String) : Boolean = {
    val symbol = extractSymbolIf(tree, {
      case ClassDef(_,_,_,_) => true
      case ModuleDef(_,_,_) => true
      case _ => false
    })

    symbol.map(_.selfType.baseClasses)
      .map(classNames => classNames.exists(symbolMatchWithName(_, typeName)))
      .exists(p => p)
  }

  /**
    * this method, starting from the root of a tree, search all the children
    * tree with some properties/structure
    * @param root: the root tree where the searching starts.
    * @param extractor: the policies that tell what to extract from the root tree.
    * @return all child extracted from the root.
    */
  protected def search(root : Tree)(extractor : Tree => Option[Tree]) : List[Tree] = extractor(root) match {
    case None => root.children.flatMap(search(_)(extractor))
    case Some(tree) => List(tree)
  }

  /**
    * It extracts, from a tree, the aggregate main definition.
    * The policy is very simple: if the tree is a DefDef and has override modifiers,
    * it is an aggregate main. A more stronger approach must evaluate where the main is
    * defined.
    * @param tree: the AST to verify.
    * @return : Some(DefDef) if the tree is an aggregate main definition, None otherwise
    */
  protected def extractAggregateMain(tree : Tree) : Option[DefDef] = {
    def isAggMain(defDef : DefDef) = defDef match {
      case q"override def main() : $tpe = $body" =>
        true
      case _ => false
    }
    tree match {
      case defMain : DefDef if isAggMain(defMain) => Some(defMain)
      case _ => None
    }
  }

  /**
    * this method is tricky, but is really important.
    * When a function uses currying, the compiler translates it in a sequence of
    * apply invocation.
    * for example, when the following method:
    * def cur(x : Int)(y : Double) : Int
    * will be invoked, it will be converted in:
    *  -- caller --   -- method --
    * (cur.apply(10)).apply(20.2)
    * for this reason, in the AST, the first argument will be 20.2.
    * To reach 10, you have to evaluate the caller that is another apply node.
    * Uncurrying tries to jump across callers as many times as specified in uncurryTimes.
    * This is very important to type check the constructor using standard notations.
    * @param apply : starting point.
    * @param uncurryTimes : jumping times.
    * @return
    */
  protected def uncurrying(apply : Apply, uncurryTimes : Int): Option[Apply] = (uncurryTimes,apply) match {
    case (0,_) => Some(apply)
    case (n, Apply(fun : Apply, _)) => uncurrying(fun, n - 1)
    case _ => None
  }
  Block
  /**
    * It extracts, from a tree, all the expressions contained. If the tree is a Block, the
    * expression is the tree itself. If the tree is a Block, return all its children.
    * @param tree: the AST from which the expressions are found
    * @return: the expression sequence extracted
    */
  protected def expressionsSequence(tree : Tree) : Seq[Tree] = tree match {
    case Block(_, _) => tree.children
    case _ => List(tree)
  }
}
object AbstractComponent {

  /**
    * set of available options, disposable through the plugin
    */
  object ComponentOption {
    val disableOption : String = "disable"
    val errorValue : String = "error"
    val allValue : String = "*"
  }
}