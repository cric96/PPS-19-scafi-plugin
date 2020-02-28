package it.unibo.scafi.plugin.component

import it.unibo.scafi.plugin.common.{AbstractComponent, ComponentContext, ComponentDescriptor}

import scala.tools.nsc.ast.TreeDSL
import scala.tools.nsc.transform.Transform
//TODO transform works before "namer" does, so there aren't any simbols. It can be built later, maybe.

//this component is a transform and modify the resulting AST.
// It is better to keep divided the two parts.
/**
  * This component modifies the AST in order to support scafi syntax.
  */
class TransformComponent(val c : ComponentContext) extends AbstractComponent(c, TransformComponent)
  with Transform
  with TreeDSL {
  import global._ //needed to avoid global.type for each compiler type

  override protected def newTransformer(unit: CompilationUnit): Transformer = {
    global.inform(phaseName)
    AggregateProgramTransformer
  }

  private object AggregateProgramTransformer extends Transformer {
    override def transform(tree: global.Tree): global.Tree = {
      extractAggregateMain(tree) match {
        case None => super.transform(tree)
        case Some(_) => WrapFunction.transform(tree)
      }
    }
  }
  //It verifies if there are lambdas and in that case it wraps them with aggregate
  private object WrapFunction extends Transformer {
    override def transform(tree: global.Tree): global.Tree = {
      tree match {
        case q"(..$args) => aggregate($body)" => super.transform(tree) //se già c'è aggregate, non si aggiunge di nuovo il mark
        case q"(..$args) => { ..$body }" => q"(..$args) => aggregate{ ..$body }" //altrimenti si wrappa la lambda con aggregate
        case _ => super.transform(tree) //in tutti gli altri casi non si fa niente
      }
    }
  }
}
object TransformComponent extends ComponentDescriptor  {
  override def name: String = "transform"

  override val runsAfter: List[String] = List("parser")

  override val runsBefore: List[String] = List("namer")

  def apply()(implicit c : ComponentContext) : TransformComponent = new TransformComponent(c)
}
