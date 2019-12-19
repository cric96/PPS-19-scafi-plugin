package it.unibo.scafi.plugin
import scala.tools.nsc.plugins.PluginComponent
/**
  * it is used to describe a component
  * @tparam P the type of component described
  */
trait ComponentDescriptor[P <: PluginComponent] {
  def name : String
  /**
    * create the specific component from the context.
    * @param context: the compilation context with scafi names used to check certain properties
    * @return the component create from the context
    */
  def apply()(implicit context : ComponentContext) : P
}
