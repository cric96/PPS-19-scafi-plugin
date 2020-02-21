package it.unibo.scafi.plugin
import it.unibo.scafi.definition.{AggregateFunction, AggregateType, F, L, T}
import AggregateFunction._

import scala.tools.nsc.Phase

class DiscoverComponent(val c : ComponentContext) extends AbstractComponent(c, DiscoverComponent){
  import global._
  import DiscoverComponent._
  var functionsDef : List[DefDef] = List.empty
  var resolvingSet : Set[String] = Set.empty

  override def newPhase(prev: Phase): Phase = new DiscoverPhase(prev)

  class DiscoverPhase(prev : Phase) extends StdPhase(prev) {
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
          case _  if name.contains("main") => false
          case _ => ! context.aggregateFunctions.contains(name)
        }
      }
      def functionAllowed(funDef : DefDef) : Boolean = !funDef.rhs.isEmpty && nameAllowed(funDef.symbol.fullName)
      tree match {
        case defDef: DefDef if functionAllowed(defDef) =>  defDef :: tree.children.flatMap(findAllConstructs)
        case _ => tree.children.flatMap(findAllConstructs)
      }
    }

    override def run(): Unit = {
      echoPhaseSummary(this)
      currentRun.units foreach applyPhase
      markAggregateConstructors()
    }

    private def markAggregateConstructors(): Unit = {
      val functionsDefMap = functionsDef.map(funDef => funDef.symbol.fullName -> funDef).toMap
      functionsDefMap.foreach(resolveType(_, functionsDefMap))
    }

    private def resolveType(defDef: (String, DefDef), functionDefMap: Map[String, DefDef]): Boolean = {
      val (name, funDef) = defDef
      global.inform("resolving:" + name)

      if(resolvingSet.contains(name)) {
        return false
      }

      resolvingSet += name

      def expressions(tree : Tree) : List[Tree] = tree match {
        case Block(_, _) => tree.children
        case _ => List(tree)
      }

      def resolveReturnType(tree : Tree) : AggregateType = {
        val lastExp = expressions(tree).last
        context.functionFromTree(lastExp) match {
          case None => T
          case Some(fun) => fun.returns
        }
      }

      def incompatibleType(types : Seq[AggregateType]) = types.contains(L) && types.contains(F)

      def typeFromCompatibleTypes(types : Seq[AggregateType]) : AggregateType = types match {
        case _ if types.contains(L) => L
        case _ if types.contains(F) => F
        case _ if types.nonEmpty => types.head
        case _ => T
      }
      //TODO CLARIFY THIS METHOD
      def extractArgTypeFrom(apply : Apply, aggFun : AggregateFunction, argDef : Tree) : Seq[AggregateType] = {
        aggFun.args.zipWithIndex.collect {
          case (argBlock, index) => val currentBlock = uncurry(apply, index)
            currentBlock.args.zipWithIndex.collect {
              case (applyArg, index) if (applyArg.symbol == argDef.symbol) =>
                try {
                  argBlock(index)
                } catch {
                  case exc : IndexOutOfBoundsException =>

                    println(index, aggFun, apply.symbol.fullName, "BLOCK = " + argBlock, "block" + currentBlock.args)
                    null
                }
            }
        }.flatten
      }
      def resolveArg(argDef : Tree) : AggregateType = {
        val bodyExpr = expressions(funDef.rhs)
        val typesFromBody = bodyExpr.collect {
          case apply : Apply => context.functionFromTree(apply) match {
            case Some(aggFun) => extractArgTypeFrom(apply, aggFun, argDef)
            case None => functionDefMap.get(apply.symbol.fullName) match {
              case Some(defDef) =>
                val resolved = resolveType(apply.symbol.fullName -> defDef, functionDefMap)
                if(resolved) {
                  extractArgTypeFrom(apply, context.functionFromTree(apply).get, argDef)
                } else {
                  List.empty
                }
              case _ => List.empty
            }
          }
        }.flatten
        if(incompatibleType(typesFromBody)) {
          error("incompatible aggregate type: pay attention in use of local and field types..")
        }
        typeFromCompatibleTypes(typesFromBody)
      }
      val args = funDef.vparamss
        .map(params => block(params.filter(! _.symbol.isImplicit).map(resolveArg):_*))
        .filter(_.nonEmpty)

      ///return type eval
      val returnType = resolveReturnType(funDef.rhs)
      val aggFunDef = AggregateFunction(name, returnType, args)
      global.inform(funDef.pos, resolveAggDefinition(aggFunDef))
      c.aggregateFunctions += name -> aggFunDef
      resolvingSet -= name
      true
    }
  }
}
object DiscoverComponent extends ComponentDescriptor  {
  override def name: String = "scafi-discover"

  override val runsBefore: List[String] = List(TypeCheckComponent.name)

  override val runsAfter: List[String] = List("refchecks")

  def resolveAggDefinition(aggFun : AggregateFunction) : String = "resolved:" + aggFun

  def apply()(implicit c : ComponentContext) : DiscoverComponent = new DiscoverComponent(c)
}