package it.unibo.scafi.definition

import it.unibo.scafi.definition.AggregateFunction._

import scala.tools.nsc.Global

/**
  * algebric data type used to represent field calculus types.
  */
sealed trait AggregateType
/**
  * L stands for "local". the type must be a local value.
 */
case object L extends AggregateType

/**
  * F stands for "field". the type must be a field value
  */
case object F extends AggregateType

/**
  * T represents a type that can be whether a field or a local value.
  */
case object T extends AggregateType

/**
  * this type represents the type of a function.
  * @param args: the type argument list.
  * @param returns: return the type of the function.
  */
case class ArrowType(args : Seq[AggregateType], returns : AggregateType) extends AggregateType {
  override def toString: String = {
    val argString = args.mkString("(",";",")")
    s"$argString=>$returns"
  }
}
/**
  * It describes the signature of an aggregate function. It is used to do
  * type checking over a standard scala type system. this object is associated with
  * some function definition.
  * example:
  *   scala definition:
  *   package it.core
  *   object Main {
  *      def func[A](f: => A)(b : => A, c : Int) : A
  *   }
  *   scafi type representation:
  *     AggregateFunction(it.core.Main.func, L, args(block(F),block(L,T))
  * @param args: a sequence of blocks, each of them is a sequence of arguments. This
  *            structure is used to represent currying functions.
  */
case class AggregateFunction(name : String, returns : AggregateType, args : Seq[ArgsBlock]) {
  val argsReversed = args.reverse

  override def toString: String = s"$name${args.map(_.mkString("(", ",", ")")).mkString("")}: ${returns}"
}

object AggregateFunction {
  //type alias used to understand args types.
  type ArgsBlock = Seq[AggregateType]

  /**
    * transform a sequence of aggregate function definitions into a map in which each function name
    * is associated with its aggregate function definition.
    */
  def toMap(aggregateFunctions: AggregateFunction *): Map[String, AggregateFunction] = {
    aggregateFunctions.map(fun => fun.name -> fun).toMap
  }

  /**
    * "dsl" function used to make argument block creation easier. It wraps a sequence or
    * an argument type into an argument function block.
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
  /**
    * shortcut of AggregateFunction constructor.
    */
  def fromSymbol(name : Global#Symbol, returns : AggregateType, args : Seq[ArgsBlock]) : AggregateFunction = {
    AggregateFunction(name.fullName, returns, args)
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