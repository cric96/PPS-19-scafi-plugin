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
      global.reporter.echo(phaseName)

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
    }

    def ifPresenceCheck(tree : Tree): Unit = tree match {
      case If(_,_,_) =>
        global.reporter.warning(tree.pos, TypeCheckComponent.ifInfoString)
        tree.children.foreach(ifPresenceCheck)
      case _ =>
        tree.children.foreach(ifPresenceCheck)
    }
  }
}

object TypeCheckComponent extends ComponentDescriptor[TypeCheckComponent] {
  override def name: String = "typecheck-aggregate"

  override def apply()(implicit c: ComponentContext): TypeCheckComponent = new TypeCheckComponent(c)

  val ifInfoString : String = "if not allowed in aggregate main"
}