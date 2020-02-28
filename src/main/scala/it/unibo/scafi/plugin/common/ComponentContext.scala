package it.unibo.scafi.plugin.common

import it.unibo.scafi.definition.{AggregateFunction, AggregateType}

import scala.tools.nsc.Global

/**
  * It is the compilation context used by the different plugins and it can change during
  * computation. It has to keep shared the global instance among all the components.
  * It also contains the main names used by the components to do their job and it contains
  * all the definitions of the aggregate functions.
  * @param global : compilation context used by every component
  * @param aggregateProgram : name associated with the root trait, where it is used to define
  *                         aggregates programs
  * @param constructs : name associated with the root trait where it is used to save functions definition
  * @param aggregateFunctions : set of aggregate functions on which perform type-checking
  */
class ComponentContext(val global : Global,
                       val aggregateProgram : String,
                       val constructs : String,
                       private val aggregateFunctions : Map[String, AggregateFunction]
                      ) {
  private var aggArgMap : Map[Global#Symbol, AggregateType] = Map.empty
  private var aggSymbolMap : Map[Global#Symbol, AggregateFunction] = Map.empty

  /**
    * It adds, to the current saved, a new aggregate function definition
    * @param symbol : the symbol associated to the function definition
    * @param aggDef : the signature associated to the function
    */
  def addAggregateFunction(symbol : Global#Symbol, aggDef : AggregateFunction): Unit = aggSymbolMap += symbol -> aggDef

  /**
    * it marks a symbol with a specific aggregated type
    * @param symbol : the symbol associated to a specific value
    * @param aggType : the type used to mark the symbol
    */
  def markSymbolWithType(symbol : Global#Symbol, aggType : AggregateType): Unit = aggArgMap += symbol -> aggType

  /**
    * It searches among aggregate functions for the one associated with the passed symbol
    * @param sym : the symbol that has to be associated with a function
    * @return Some(aggFun) if exists a function associated with the symbol, None otherwise
    */
  def extractAggFunctionFromSymbol(sym : Global#Symbol) : Option[AggregateFunction] = aggregateFunctions.get(sym.fullName).orElse(aggSymbolMap.get(sym))

  /**
    * It checks if there is a match between an aggregate type and the passed symbol.
    * In that case it return the associated type
    * @param symbol : the symbol to search for a match
    * @return Some(aggType) if exists a type associated with the symbol, None otherwise
    */
  def extractTypeFromSymbol(symbol : Global#Symbol) : Option[AggregateType] = aggArgMap.get(symbol)

  /**
    * Given an AST, it verifies if it is associated with a function definition.
    * If yes, it return that function definition
    * @param tree : the AST to check
    * @return Some(aggFun) if exists an association between the AST and the aggregate function, None otherwise
    */
  def extractAggFunctionFromTree(tree : Global#Tree): Option[AggregateFunction] = tree.symbol match {
    case null => None
    case sym => aggregateFunctions.get(sym.fullName).orElse(aggSymbolMap.get(sym))
  }
}

