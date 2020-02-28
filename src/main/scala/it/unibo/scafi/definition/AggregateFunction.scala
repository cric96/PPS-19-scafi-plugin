package it.unibo.scafi.definition

import it.unibo.scafi.definition.AggregateFunction._
/**
  * algebric data type used to represent field calculus types.
  */
sealed trait AggregateType {

}
/**
  * L stands for "local". the type must be a local value.
 */
case object L extends AggregateType

/**
  * F stands for "field. the type must be a field value
  */
case object F extends AggregateType

/**
  * T represent a type that can be field or local.
  */
case object T extends AggregateType

/**
  * this type represent the type of a function.
  * @param args: the argument type list.
  * @param returns: return type of function.
  */
case class ArrowType(args : Seq[AggregateType], returns : AggregateType) extends AggregateType {
  override def toString: String = {
    val argString = args.mkString("(",";",")")
    s"$argString=>$returns"
  }
}
//TODO explain better

/**
  * describe the signature of an aggregate function. It is used to do
  * type check over a standard scala type system. this object is associated with
  * some function definition.
  * example:
  *   scala definition:          def func[A](f: => A)(b : => A, c : Int) : A
  *   scafi type representation: AggregateFunction(func, L, args(block(F),block(L,T))
  * @param args: a sequence of function argument blocks. each block is a sequence of argument. This
  *            structure is used to represent curried function.
  */
case class AggregateFunction(name : String, returns : AggregateType, args : Seq[ArgsBlock]) {
  val argsReversed = args.reverse

  override def toString: String = s"$name${args.map(_.mkString("(", ",", ")")).mkString("")}: ${returns}"
}

object AggregateFunction {
  //type alias used to understand args types.
  type ArgsBlock = Seq[AggregateType]

  /**
    * transform a sequence of aggregate function definition into a map in which each name
    * is associated with its aggregate function definition.
    */
  def toMap(aggregateFunctions: AggregateFunction *): Map[String, AggregateFunction] = {
    aggregateFunctions.map(fun => fun.name -> fun).toMap
  }

  /**
    * "dsl" function used to make argument block creation easier. wrap a sequence or argument
    * type into a argument function block.
    */
  def block(types : AggregateType *) : ArgsBlock = types

  /**
    * "dsl" function used to make argument block list creation easier.
    */
  def args(blocks : ArgsBlock *) : Seq[ArgsBlock] = blocks

  /**
    * shortcut of AggregateFunction constructor.
    */
  def aggFun(name : String, returns : AggregateType, args : Seq[ArgsBlock]) : AggregateFunction = {
    AggregateFunction(name, returns, args)
  }
  private def productToSeq(p : Product) : Seq[AggregateType] = p.productIterator.toList.map {
    case value : AggregateType => value
  }
  /*dsl utilities used to write arrow type*/
  implicit def tuple1ToArrowType(tpe : (AggregateType, AggregateType)) : ArrowType = ArrowType(List(tpe._1), tpe._2)
  implicit def tuple2ToArrowType(tpe : ((AggregateType, AggregateType), AggregateType)) : ArrowType = ArrowType(productToSeq(tpe._1),tpe._2)
  implicit def tuple3ToArrowType(tpe : ((AggregateType, AggregateType, AggregateType), AggregateType)) : ArrowType = ArrowType(productToSeq(tpe._1), tpe._2)
  implicit def tuple4ToArrowType(tpe : ((AggregateType, AggregateType, AggregateType, AggregateType), AggregateType)) : ArrowType = ArrowType(productToSeq(tpe._1), tpe._2)
  implicit def tupleSeqToArrowType(tpe : (Seq[AggregateType], AggregateType)) : ArrowType = ArrowType(tpe._1, tpe._2)

}