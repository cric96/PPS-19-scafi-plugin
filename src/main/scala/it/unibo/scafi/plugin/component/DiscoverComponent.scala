package it.unibo.scafi.plugin.component

import it.unibo.scafi.definition._
import it.unibo.scafi.plugin.common.{AbstractComponent, ComponentContext, ComponentDescriptor}

import scala.tools.nsc.{Global, Phase}
/*
  tips for solving overloading methods:
    matching with tree need another parameter, the position where the symbol is defined. with this, the
    function are unique.
 */
/**
  * this component has to search for function definition in a specific trait, starting from
  * a set of aggregate functions.
  */
class DiscoverComponent(val c : ComponentContext) extends AbstractComponent(c, DiscoverComponent){
  import DiscoverComponent._
  import global._
  var functionsDef : List[DefDef] = List.empty //insieme di funzioni aggregate da marcare
  var resolvingSet : Set[Symbol] = Set.empty //set usato per evitare problemi associati alla ricorsione

  override def newPhase(prev: Phase): Phase = new DiscoverPhase(prev)
  var i = 0
  class DiscoverPhase(prev : Phase) extends StdPhase(prev) {
    override def run(): Unit = {  //run viene chiamato in cascata alla fase precedente, in questo punto accedo (virtualmente) a tutti i sorgenti
      echoPhaseSummary(this)
      currentRun.units foreach applyPhase //chiamo apply per ogni file di compilazione
      markAggregateConstructors() //effettuo il marking dei vari costrutti trovati
    }

    private def extractConstructsDefinition(tree: Tree): Option[Tree] = Some(tree).filter(extendsFromType(_, context.constructs))

    //find all aggregate function definitions
    override def apply(unit: global.CompilationUnit): Unit = {
      inform(phaseName)
      val functionsFoundInUnit = search(unit.body)(extractConstructsDefinition) flatMap {
        findAllConstructs
      }
      functionsDef :::= functionsFoundInUnit
    }

    private def findAllConstructs(tree: Tree): Seq[DefDef] = {
      def nameAllowed(symbol : Global#Symbol) : Boolean = {
        symbol.fullName match {
          case name if name.contains("init") => false
          case name if name.contains("apply") => false
          case name if name.contains("main") => false
          case _ => context.extractAggFunctionFromSymbol(symbol).isEmpty
        }
      }

      def functionAllowed(funDef : DefDef) : Boolean = !funDef.rhs.isEmpty && nameAllowed(funDef.symbol)

      tree match {
        case defDef: DefDef if functionAllowed(defDef) => defDef :: tree.children.flatMap(findAllConstructs) //dentro potrebbero essere altre definizioni a funzioni
        case _ => tree.children.flatMap(findAllConstructs)
      }
    }
    //marking all the retrieved functions
    private def markAggregateConstructors(): Unit = {
      val nameLinkToFunction = functionsDef.map(funDef => funDef.symbol -> funDef).toMap
      functionsDef.foreach(resolveType(_, nameLinkToFunction))
    }

    private def resolveType(funDef: DefDef, nameLinkToFunction: Map[Symbol, DefDef]): Boolean = {
      //TODO improve! it doesn't evaluate if the last expression is a simple value
      def resolveReturnType(tree : Tree) : AggregateType = {
        val lastExp = expressionsSequence(tree).last
        context.extractAggFunctionFromTree(lastExp) match {
          case None => T
          case Some(fun) => fun.returns
        }
      }
      //TODO improve, expand each block during evaluation,
      def resolveArg(argDef : Tree) : AggregateType = {
        val bodyExprs = expressionsSequence(funDef.rhs)
        val typesFounded = extractAllArgType(bodyExprs, argDef)

        if(incompatibleType(typesFounded)) {
          error(incompatibleTypeError(argDef.symbol.nameString), argDef.pos)
        }
        val aggType = typeFromCompatibleTypes(typesFounded)
        context.markSymbolWithType(argDef.symbol, aggType)
        aggType
      }

      def extractAllArgType(bodyExpressions : Seq[Tree], argDef : Tree) : Seq[AggregateType] = bodyExpressions.collect {
        case functionCall : Apply => context.extractAggFunctionFromTree(functionCall) match {
          case Some(aggFun) => extractArgTypeFromApply(functionCall, aggFun, argDef) //funzione già risolta, estraggo i tipi
          case None => //non c'è ancora una funzione aggregate associata
            nameLinkToFunction.get(functionCall.symbol) match {
              case Some(aggUnsolvedFunction) => resolveInDepth(aggUnsolvedFunction, functionCall, argDef) //ne esiste una da risolve, allora vado nella risoluzione in profondità
              case _ => List.empty
            }
        }
      }.flatten

      def resolveInDepth(aggUnsolvedFunction : DefDef, functionCall : Apply, argDef : Tree): Seq[AggregateType] = {
        if (resolveType(aggUnsolvedFunction, nameLinkToFunction)) { //risolvo la funzione non marcata
          extractArgTypeFromApply(functionCall, context.extractAggFunctionFromTree(functionCall).get, argDef)
        } else {
          List.empty //se non riesco a marcarla, non possono dire niente e quindi restituisco empty
        }
      }

      def extractArgTypeFromApply(apply : Apply, aggDef : AggregateFunction, argDef : Tree) : Seq[AggregateType] = {
        val blocksUncurried = aggDef.argsReversed.zipWithIndex.map {
          case (argsBlockAgg, index) => (argsBlockAgg, uncurrying(apply, index))
        }.collect {
          case (argsBlockAgg, Some(argTree)) => (argsBlockAgg, argTree.args.zipWithIndex)
        }
        blocksUncurried.collect {
          case (argsBlockAgg, args) => args.collect {
            case (arg, index) if(arg.symbol == argDef.symbol) => argsBlockAgg(index)
          }
        }.flatten
      }

      //due recursion problems, this condiction allow to avoid stack overflow in case of nested function recursion
      if(resolvingSet.contains(funDef.symbol)) {
        return false
      }
      resolvingSet += funDef.symbol

      val args = funDef.vparamss
        .map(params => AggregateFunction.block(params.map(resolveArg):_*))
        .filter(_.nonEmpty)

      ///return type eval
      val returnType = resolveReturnType(funDef.rhs)
      val aggFunDef = AggregateFunction.fromSymbol(funDef.symbol, returnType, args)
      global.inform(funDef.pos, resolveAggDefinition(aggFunDef))
      c.addAggregateFunction(funDef.symbol, aggFunDef)
      resolvingSet -= funDef.symbol
      true
    }
  }

  private def typeFromCompatibleTypes(types : Seq[AggregateType]) : AggregateType = types match {
    case _ if types.contains(L) => L
    case _ if types.contains(F) => F
    case _ if types.nonEmpty => types.head
    case _ => T
  }

  private def incompatibleType(types : Seq[AggregateType]) = types.contains(L) && types.contains(F)
}
object DiscoverComponent extends ComponentDescriptor  {
  override def name: String = "discover"

  override val runsBefore: List[String] = List(TypeCheckComponent.name)

  override val runsAfter: List[String] = List("pickler")

  def resolveAggDefinition(aggFun : AggregateFunction) : String = "resolved:" + aggFun

  def incompatibleTypeError(arg : String) : String = s"incompatible aggregate type in $arg: pay attention in use of local and field types.."

  def apply()(implicit c : ComponentContext) : DiscoverComponent = new DiscoverComponent(c)
}