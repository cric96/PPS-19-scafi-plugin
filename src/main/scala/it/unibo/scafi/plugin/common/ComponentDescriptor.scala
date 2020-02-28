package it.unibo.scafi.plugin.common

/**
  * it is used to describe a component
  */
trait ComponentDescriptor {
  def name : String

  def runsAfter : List[String] = List()

  def runsBefore : List[String] = List()
  /**
    * create the specific component from the context.
    * @param context: the compilation context with essential information
    *               used to check certain properties
    * @return the component created from the context
    */
  def apply()(implicit context : ComponentContext) : AbstractComponent
}
