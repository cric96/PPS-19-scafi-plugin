package it.unibo.scafi.plugin
import it.unibo.scafi.definition.{AggregateFunction, AggregateType, F, L, T}
import AggregateFunction._

import scala.tools.nsc.Phase

class DiscoverComponent(val c : ComponentContext) extends AbstractComponent(c, DiscoverComponent){
  import global._
  import DiscoverComponent._
  var functionsDef : List[DefDef] = List()

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
          case "$init$" | "<init>" | "main" => false
          case _ => ! context.aggregateFunctions.contains(name)
        }
      }
      def functionAllowed(funDef : DefDef) : Boolean = !funDef.rhs.isEmpty && nameAllowed(funDef.name.toString)
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
      val functionsDefMap = functionsDef.map(funDef => funDef.symbol.nameString -> funDef).toMap
      functionsDefMap.foreach(resolveType(_, functionsDefMap))
    }

    private def resolveType(defDef: (String, DefDef), functionDefMap: Map[String, DefDef]): Unit = {
      val (name, funDef) = defDef
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
      def extractArgTypeFrom(apply : Apply, aggFun : AggregateFunction, argDef : Tree) : Seq[AggregateType] = {
        aggFun.args.zipWithIndex.collect {
          case (argBlock, index) => val currentBlock = uncurry(apply, index)
            currentBlock.args.zipWithIndex.collect {
              case (applyArg, index) if (applyArg.symbol == argDef.symbol) => argBlock(index)
            }
        }.flatten
      }
      def resolveArg(argDef : Tree) : AggregateType = {
        val bodyExpr = expressions(funDef.rhs)
        val typesFromBody = bodyExpr.collect {
          case apply : Apply => context.functionFromTree(apply) match {
            case Some(aggFun) => extractArgTypeFrom(apply, aggFun, argDef)
            case None => functionDefMap.get(apply.symbol.nameString) match {
              case Some(defDef) =>
                resolveType(apply.symbol.nameString -> defDef, functionDefMap)
                extractArgTypeFrom(apply, context.functionFromTree(apply).get, argDef)
              case _ => List.empty
            }
          }
        }.flatten
        if(incompatibleType(typesFromBody)) {
          error("incompatible aggregate type: pay attention in use of local and field types..")
        }
        typeFromCompatibleTypes(typesFromBody)
      }
      ///first expand each tree.
      val args = funDef.vparamss.map(params => block(params.map(resolveArg):_*))
      ///return type eval
      val returnType = resolveReturnType(funDef.rhs)
      val aggFunDef = AggregateFunction(name, returnType, args)
      global.inform(resolveAggDefinition(aggFunDef))
      c.aggregateFunctions += name -> aggFunDef
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