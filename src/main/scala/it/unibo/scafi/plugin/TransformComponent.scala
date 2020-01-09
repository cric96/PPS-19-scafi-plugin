package it.unibo.scafi.plugin

import scala.tools.nsc.ast.TreeDSL
import scala.tools.nsc.transform.Transform

//TODO: weak approach, working after parser phase makes this method problematic, think how you can generalize it.
class TransformComponent(val c : ComponentContext) extends AbstractComponent(c)
  with Transform
  with TreeDSL {
  import global._ //needed to avoid global.type for each compiler type
  override val phaseName: String = TransformComponent.name
  override val runsAfter: List[String] = List("parser")
  override val runsBefore: List[String] = List("namer")
  //TODO: weak way to verify aggregate function. improve it.
  protected def aggregateFun(tree : Tree) : Option[Tree] = tree match {
    case apply : Apply =>
      val funName = apply.toString().split('(')(0)
      context.aggregateFunctions.get(funName) match {
        case None => None
        case Some(_) => Some(tree)
      }
    case _ => None
  }
  override protected def newTransformer(unit: CompilationUnit): Transformer = {
    global.inform(phaseName)
    AggregateProgramTransformer
  }

  private object AggregateProgramTransformer extends Transformer {
    val aggregateProgramTransform = WrapFunction
    override def transform(tree: global.Tree): global.Tree = {
      aggregateFun(tree) match {
        case None => {
          super.transform(tree)
        }
        case Some(_) => {
          aggregateProgramTransform.transform(tree)
        }
      }
    }
  }

  private object WrapFunction extends Transformer {
    override def transform(tree: global.Tree): global.Tree = {
      tree match {
        case fun : Function =>
          val wrapped = Function(fun.vparams, q"aggregate{..${fun.body}}")
          wrapped
        case _ => super.transform(tree)
      }
    }
  }
}

object TransformComponent extends ComponentDescriptor[TransformComponent]  {
  def apply()(implicit c : ComponentContext) : TransformComponent = new TransformComponent(c)

  override def name: String = "scafi-transform"
}
