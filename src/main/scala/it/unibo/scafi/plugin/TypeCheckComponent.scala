package it.unibo.scafi.plugin

import scala.tools.nsc.Phase

/**
  * check if, in the aggregate program (or in constructor definition(*?*)) certain
  * properties are satisfied. Examples of properties are:
  *   - if not allowed inside aggregate main(?)
  *   - type checking in the aggregate constructor ( nbr(nbr(x)) mustn't compile
  *   - ...
  */
class TypeCheckComponent(context : ComponentContext) extends CommonComponent(context) {
  import global._
  import TypeCheckComponent._
  override val phaseName: String = TypeCheckComponent.name
  override val runsAfter: List[String] = List("refchecks")

  override def newPhase(prev: Phase): Phase = new TypeCheckPhase(prev)
  /**
    * the phases used to check the type checking correctness.
    * @param prev the previous phase in the compilation pipeline
    */
  class TypeCheckPhase(prev: Phase) extends StdPhase(prev) {
    override def name = TypeCheckComponent.this.phaseName
    //here the magic happens, verify the correctness of the programs
    override def apply(unit: CompilationUnit): Unit = {
      global.inform(phaseName)
      def extractMain(tree : Tree) : Option[Tree] = tree match {
        case DefDef(_,name,_,_,_,_) if name.containsName("main") => Some(tree)
        case _ => tree.children.map(extractMain).collectFirst {
          case Some(childTree) => childTree
        }
      }
      //extract all aggregate main, and check the properties
      extractAggregatePrograms(unit.body).map(extractMain).collect {
        case Some(mainTree) => mainTree
      } foreach {
        evalAggregateMain  //here starts program evaluation
      }
    }

    private def evalAggregateMain(tree : Tree) : Unit = {
      //in this def, there are all the checking in the programs
      ifPresenceCheck(tree)
      nbrNestedCheck(tree)
      foldhoodAndRepCorrectness(tree)
    }

    private def ifPresenceCheck(tree : Tree): Unit = tree match {
      case If(_,_,_) =>
        warning(tree.pos, ifInfoString)
        tree.children.foreach(ifPresenceCheck)
      case _ =>
        tree.children.foreach(ifPresenceCheck)
    }

    private def nbrNestedCheck(tree : Tree) : Unit = extractByApplyName(tree, context.names.nbr) match {
      case Some(apply) => if(apply.args(firstArg).exists(nbrPresence))
        globalError(tree.pos, nbrNestedErrorString)
      case _ =>  tree.children.foreach(nbrNestedCheck)
    }

    private def nbrPresence(tree : Tree) : Boolean = if (isNbr(tree)) {
      true
    } else {
      tree.children.exists(nbrPresence)
    }

    private def isNbr(tree : Tree) : Boolean = sameApplyName(tree, context.names.nbr)

    private def foldhoodAndRepCorrectness(tree : Tree) : Unit = {
      val foldHoodApply = extractByApplyName(tree, context.names.hood)
      val repApply = extractByApplyName(tree, context.names.rep)

      (foldHoodApply, repApply) match {
        case (Some(apply), None) =>
          val uncurried = uncurry(apply, curryingFoldTimes)
          if(nbrPresence(uncurried.args(firstArg))) {
            globalError(tree.pos, foldHoodErrorString)
          }
        case (None, Some(apply)) =>
          val uncurried = uncurry(apply, curryingRepTimes)
          if(nbrPresence(uncurried.args(firstArg))) {
            globalError(tree.pos, repErrorString)
          }
        case _ => tree.children.foreach{foldhoodAndRepCorrectness}
      }
    }

    private def extractByApplyName(tree : Tree, name: String) : Option[Apply] = tree match {
      case res : Apply if hasSameName(tree.symbol, name) => Some(res)
      case _ => None
    }

    private def sameApplyName(tree : Tree, name : String) : Boolean = extractByApplyName(tree, name).nonEmpty

    private def uncurry(apply : Apply, uncurryTimes : Int): Apply = (uncurryTimes,apply) match {
      case (0,_) => apply
      case (n, Apply(fun : Apply, _)) => uncurry(fun, n - 1)
      case _ => apply
    }
  }
}

object TypeCheckComponent extends ComponentDescriptor[TypeCheckComponent] {
  override def name: String = "typecheck-aggregate"

  override def apply()(implicit c: ComponentContext): TypeCheckComponent = new TypeCheckComponent(c)

  val ifInfoString : String = "if not allowed in aggregate main"

  val nbrNestedErrorString : String = "error, nbr of nbr not allowed in aggregate main"

  val foldHoodErrorString : String = "error, first value of foldhood must be local, not field"

  val repErrorString : String = "error, first value of rep must be local, not field"

  val firstArg : Int = 0

  val curryingFoldTimes : Int = 2

  val curryingRepTimes : Int = 1
}