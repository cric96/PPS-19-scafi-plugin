package it.unibo.scafi.plugin
import it.unibo.scafi.definition.{AggregateFunction, AggregateType, F, L, T}
import AggregateFunction._

import scala.tools.nsc.Phase
/*
  tips for solving overloading methods:
    matching with tree need another parameter, the position where the symbol is defined. with this, the
    function are unique.
 */
/**
  *
  * @param c
  */
class DiscoverComponent(val c : ComponentContext) extends AbstractComponent(c, DiscoverComponent){
  import global._
  import DiscoverComponent._
  var functionsDef : List[DefDef] = List.empty
  var resolvingSet : Set[Symbol] = Set.empty

  override def newPhase(prev: Phase): Phase = new DiscoverPhase(prev)

  class DiscoverPhase(prev : Phase) extends StdPhase(prev) {
    override def run(): Unit = {
      echoPhaseSummary(this)
      currentRun.units foreach applyPhase
      markAggregateConstructors()
    }

    private def extractConstructsDefinition(tree: Tree): Option[Tree] = Some(tree).filter(extendsFromType(_, context.constructs))

    //find all aggregate function definitions
    override def apply(unit: global.CompilationUnit): Unit = {
      inform(phaseName)
      val functionFoundInUnit = search(unit.body)(extractConstructsDefinition) flatMap {
        findAllConstructs
      }
      functionsDef :::= functionFoundInUnit
    }

    private def findAllConstructs(tree: Tree): Seq[DefDef] = {
      def nameAllowed(name : String) : Boolean = {
        name match {
          case _  if name.contains("init") => false
          case _  if name.contains("apply") => false
          case _  if name.contains("main") => false
          case _ => context.extractAggFunctionFromName(name).isEmpty
        }
      }

      def functionAllowed(funDef : DefDef) : Boolean = !funDef.rhs.isEmpty && nameAllowed(funDef.symbol.fullName)

      tree match {
        case defDef: DefDef if functionAllowed(defDef) => defDef :: tree.children.flatMap(findAllConstructs)
        case _ => tree.children.flatMap(findAllConstructs)
      }
    }

    private def markAggregateConstructors(): Unit = {
      val nameLinkToFunction = functionsDef.map(funDef => funDef.symbol -> funDef).toMap
      functionsDef.foreach(resolveType(_, nameLinkToFunction))
    }

    private def resolveType(funDef: DefDef, nameLinkToFunction: Map[Symbol, DefDef]): Boolean = {
      def resolveReturnType(tree : Tree) : AggregateType = {
        val lastExp = expressionsSequence(tree).last
        context.extractAggFunctionFromTree(lastExp) match {
          case None => T
          case Some(fun) => fun.returns
        }
      }
      //TODO improve, expand each block during valutation, clarify
      def resolveArg(argDef : Tree) : AggregateType = {
        val bodyExprs = expressionsSequence(funDef.rhs)
        val typesFounded = extractAllArgType(bodyExprs, argDef)

        if(incompatibleType(typesFounded)) {
          error(incompatibleTypeError(argDef.symbol.nameString), argDef.pos)
        }
        val aggType = typeFromCompatibleTypes(typesFounded)
        context.addArgumentType(argDef.symbol, aggType)
        aggType
      }

      def extractAllArgType(bodyExpressions : Seq[Tree], argDef : Tree) : Seq[AggregateType] = bodyExpressions.collect {
        case functionCall : Apply => context.extractAggFunctionFromTree(functionCall) match {
          case Some(aggFun) => extractArgTypeFrom(functionCall, aggFun, argDef)
          case None =>
            nameLinkToFunction.get(functionCall.symbol) match {
              case Some(aggUnsolvedFunction) => resolveInDepth(aggUnsolvedFunction, functionCall, argDef)
              case _ => List.empty
            }
        }
      }.flatten

      def resolveInDepth(aggUnsolvedFunction : DefDef, functionCall : Apply, argDef : Tree): Seq[AggregateType] = {
        if (resolveType(aggUnsolvedFunction, nameLinkToFunction)) {
          extractArgTypeFrom(functionCall, context.extractAggFunctionFromTree(functionCall).get, argDef)
        } else {
          List.empty
        }
      }

      //TODO CLARIFY THIS METHOD
      def extractArgTypeFrom(apply : Apply, aggDef : AggregateFunction, argDef : Tree) : Seq[AggregateType] = {
        aggDef.argsReversed.zipWithIndex.collect {
          case (argBlock, index) =>
            val currentBlock = uncurry(apply, index)
            currentBlock.args.zipWithIndex.collect {
              case (applyArg, index) if (applyArg.symbol == argDef.symbol) => argBlock(index)
            }
        }.flatten
      }

      //due recursion problems, this condiction allow to avoid stack overflow in case of nested function recursion
      if(resolvingSet.contains(funDef.symbol)) {
        return false
      }
      resolvingSet += funDef.symbol

      val args = funDef.vparamss
        .map(params => block(params.map(resolveArg):_*))
        .filter(_.nonEmpty)

      ///return type eval
      val returnType = resolveReturnType(funDef.rhs)
      val aggFunDef = AggregateFunction(funDef.symbol.fullName, returnType, args)
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

  override val runsAfter: List[String] = List("refchecks")

  def resolveAggDefinition(aggFun : AggregateFunction) : String = "resolved:" + aggFun

  def incompatibleTypeError(arg : String) : String = s"incompatible aggregate type in $arg: pay attention in use of local and field types.."

  def apply()(implicit c : ComponentContext) : DiscoverComponent = new DiscoverComponent(c)
}