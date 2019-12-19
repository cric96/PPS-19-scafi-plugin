package it.unibo.scafi.plugin

import scala.tools.nsc.ast.TreeDSL
import scala.tools.nsc.transform.Transform

//TODO: creation of the structure, thing what I need to transform
class TransformComponent(val c : ComponentContext) extends CommonComponent(c)
  with Transform
  with TreeDSL {
  import global._ //needed to avoid global.type for each compiler type
  override val phaseName: String = TransformComponent.name
  override val runsAfter: List[String] = List(TypeCheckComponent.name)

  override protected def newTransformer(unit: CompilationUnit): Transformer = {
    global.reporter.echo(phaseName)
    new Transformer //TODO
  }
}

object TransformComponent extends ComponentDescriptor[TransformComponent]  {
  def apply()(implicit c : ComponentContext) : TransformComponent = new TransformComponent(c)

  override def name: String = "scafi-transform"
}
