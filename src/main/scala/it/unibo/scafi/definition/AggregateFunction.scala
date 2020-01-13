package it.unibo.scafi.definition

import it.unibo.scafi.definition.AggregateFunction._
sealed trait AggregateType
case object L extends AggregateType
case object F extends AggregateType
case object T extends AggregateType
//TODO explain better

/**
  * describe the signature of an aggregate function.
  * @param args: ordered sequence of argument block.
  */
case class AggregateFunction(name : String, returns : AggregateType, args : Seq[ArgsBlock]) {
  val argsReversed = args.reverse

  override def toString: String = s"$name${args.flatten.mkString("(", ",", ")")}: ${returns}"
}

object AggregateFunction {
  type ArgsBlock = Seq[AggregateType]
  def toMap(aggregateFunction: AggregateFunction *): Map[String, AggregateFunction] = {
    Tuple1
    aggregateFunction.map(fun => fun.name -> fun).toMap
  }

  def block(types : AggregateType *) : ArgsBlock = types

  def args(blocks : ArgsBlock *) : Seq[ArgsBlock] = blocks

  def aggFun(name : String, returns : AggregateType, args : Seq[ArgsBlock]) : AggregateFunction = {
    AggregateFunction(name, returns, args)
  }
}