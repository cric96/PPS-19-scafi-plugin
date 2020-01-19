package it.unibo.scafi.plugin
import it.unibo.scafi.definition.{AggregateFunction, AggregateType, T}
import AggregateFunction._
import scala.tools.nsc.Phase

class DiscoverComponent(val c : ComponentContext) extends AbstractComponent(c, DiscoverComponent){
  import global._
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
      println(functionsDef.map(_.name))
      markAggregateConstructors()
      println(context.aggregateFunctions)
    }

    private def markAggregateConstructors(): Unit = {
      val functionsDefMap = functionsDef.map(funDef => funDef.symbol.nameString -> funDef).toMap
      functionsDefMap.foreach(resolveType(_, functionsDefMap))
    }

    private def resolveType(defDef: (String, DefDef), functionDefMap: Map[String, DefDef]): Unit = {
      val (name, funDef) = defDef

      def isAlreadyMarked(tree : Tree) : Boolean = context.functionFromSymbol(tree.symbol).isDefined
      def expressions(tree : Tree) : List[Tree] = tree match {
        case Block(_, _) => tree.children
        case _ => List(tree)
      }
      def resolveReturnType(tree : Tree) : AggregateType = {
        val lastExp = expressions(tree).last
        context.functionFromSymbol(lastExp.symbol) match {
          case None => T
          case Some(fun) => fun.returns
        }
      }
      def resolveArg(arg : Tree) : AggregateType = {
        var argType : AggregateType = T
        val exprs = expressions(funDef.rhs)
        exprs foreach {
          case ap: Apply if isAlreadyMarked(ap) => T //TODO
          case ap: Apply if functionDefMap.contains(ap.symbol.nameString) => T //TODO
          case expr => T
        }
        argType
      }
      val args = funDef.vparamss.map(params => block(params.map(resolveArg):_*))
      ///first expand each tree.
      ///return type eval
      val returnType = resolveReturnType(funDef.rhs)
      c.aggregateFunctions += name -> AggregateFunction(name, returnType, args)
    }

    private def extractReturnType(tree: Tree): AggregateType = tree.symbol match {
      case null => T
      case symbol => context.functionFromSymbol(symbol) match {
        case None => T
        case Some(fun) => fun.returns
      }
    }
  }
}
object DiscoverComponent extends ComponentDescriptor  {
  override def name: String = "scafi-discover"

  override val runsBefore: List[String] = List(TypeCheckComponent.name)

  override val runsAfter: List[String] = List("refchecks")

  def apply()(implicit c : ComponentContext) : DiscoverComponent = new DiscoverComponent(c)
}