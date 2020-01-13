package it.unibo.scafi.plugin
import scala.tools.nsc.Phase

class DiscoverComponent(val c : ComponentContext) extends AbstractComponent(c, DiscoverComponent){
  import global._
  var functionsDef : List[c.global.Tree] = List()

  override def newPhase(prev: Phase): Phase = new DiscoverPhase(prev)

  class DiscoverPhase(prev : Phase) extends StdPhase(prev) {
    override def apply(unit: global.CompilationUnit): Unit = {
      inform(phaseName)
    }

    override def run(): Unit = {
      echoPhaseSummary(this)
      //todo find a way to do this.
      currentRun.units foreach applyPhase
    }
  }
}
object DiscoverComponent extends ComponentDescriptor  {
  override def name: String = "scafi-discover"

  override val runsBefore: List[String] = List(TypeCheckComponent.name)

  override val runsAfter: List[String] = List("refchecks")

  def apply()(implicit c : ComponentContext) : DiscoverComponent = new DiscoverComponent(c)
}