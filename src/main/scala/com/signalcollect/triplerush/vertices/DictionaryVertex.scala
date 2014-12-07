package com.signalcollect.triplerush.vertices

import scala.collection.mutable.HashMap
import com.signalcollect.GraphEditor
import com.signalcollect.triplerush.FilterTriple
import com.signalcollect.triplerush.EfficientIndexPattern
import com.signalcollect.triplerush.EfficientIndexPattern.longToIndexPattern
import com.signalcollect.triplerush.TrGlobal
import com.signalcollect.triplerush.TriplePattern
import com.signalcollect.triplerush.QueryIds
import com.signalcollect.triplerush.QueryParticle
import com.signalcollect.triplerush.QueryParticle.arrayToParticle
import com.signalcollect.util.SplayIntSet

// FilterTriple case classes / traits
import com.signalcollect.triplerush.UnaryExpression
import com.signalcollect.triplerush.BuiltInCall
import com.signalcollect.triplerush.Var
import com.signalcollect.triplerush.NumericLiteral

import com.signalcollect.triplerush.MultiplicativeExpression
import com.signalcollect.triplerush.AdditiveExpression
import com.signalcollect.triplerush.RelationalExpression
import com.signalcollect.triplerush.ConditionalAndExpression

import com.signalcollect.triplerush.Constraint
import com.signalcollect.triplerush.ConditionalOrExpression
import com.signalcollect.triplerush.GlobalNegative

final class DictionaryVertex extends IndexVertex(Long.MaxValue) {
  
  val d = TrGlobal.dictionary
  
  val filterList = HashMap.empty[Int, Seq[FilterTriple]];
  
  // Unused methods that must be implemented because of IndexVertex
  override def addChildDelta(delta: Int): Boolean = false
  override def cardinality: Int = 0
  override def foreachChildDelta(f: Int => Unit): Unit = { }
  override def handleChildIdRequest(requestor: Long, graphEditor: GraphEditor[Long, Any]): Unit = { }

  override def processQuery(query: Array[Int], graphEditor: GraphEditor[Long, Any]) {
    //checkAndForward(query, graphEditor)
  }
  
//  /**
//   * Registers filter list for a given query ID for later processing
//   */
//  override def registerFilters(queryId: Int, queryFilters: Seq[FilterTriple]) {
//    println(s"DV::registerFilters: Registering $queryFilters to query ID $queryId")
//    filterList(queryId) = queryFilters
//    summarizeNoVarFilters(queryId)
//  }
  
//  /**
//   * Checks & removes all filters with no variables and adds a
//   * "global false" filter at the beginning if the query can
//   * not succeed (i.e. if a no var filter returned false)
//   */
//  def summarizeNoVarFilters(queryId: Int) {
//    val noVarPassed = removeNoVarFilters(queryId)
//    if (!noVarPassed) {
//      filterList(queryId) = FilterTriple.globalFalse +: filterList(queryId)
//      println(filterList(queryId))
//    }
//  }
//  
//  /**
//   * Returns whether a filter doesn't make use of any variables
//   */
//  private def isNoVarFilter(filter: FilterTriple): Boolean = false // TODO 
//  
//  /**
//   * Returns a set of all variables that are required to evaluate
//   * this filter triple
//   */
//  private def getRequiredVariables(filter: FilterTriple): Seq[String] = {
//    if (filter.constraint.isInstanceOf[ConditionalOrExpression]) {
//      val condOrExpr = filter.constraint.asInstanceOf[ConditionalOrExpression]
//      condOrExpr.entries.foreach {
//        condAnd => condAnd.entries.foreach {
//          relExpr => (relExpr.rhs.get :+ relExpr.lhs).foreach {
//            
//          }
//        }
//        
//      }
//    }
//    Seq() // temp
//  }
//  
//  /**
//   * Checks & removes filters with no variables (i.e. filters that
//   * can be checked at the beginning) and return result
//   */
//  private def removeNoVarFilters(queryId: Int): Boolean = {
//    var i = 0
//    var result = true
//    filterList(queryId).foreach { e =>
//      if (isNoVarFilter(e)) {
//        if (result && !e.passes(None, None)) {
//          println(s"Filter $e did not pass!")
//          result = false
//        }
//        removeFilterFromList(queryId, i)
//        i -= 1 // compensate for filter being removed
//      }
//      i += 1
//    }
//    result
//  }
//  
//  /**
//   * Helper method to remove an item from a list with a given index
//   */
//  private def removeFilterFromList(queryId: Int, index: Int) {
//    filterList(queryId) = filterList(queryId).take(index) ++ filterList(queryId).drop(index+1)
//  }
//  
//  /**
//   * Removes filter list of a given query ID
//   */
//  def removeFilters(queryId: Int) {
//    filterList -= queryId
//  }
//  
//  /**
//   * Checks all filters that require information from newly
//   * bound variables
//   */
//  def checkAllFilters(query: Array[Int], newBindings: Array[Int]): Boolean = {
//    if (filterList(query.queryId).length > 0 && filterList(query.queryId)(0).isGlobalFalse) {
//      return false
//    }    
//    for (i <- 0 until filterList(query.queryId).length) {
//      println("Checking " + filterList(query.queryId)(i))
//      if (isRelevantFilter(query, newBindings, i) && !passesFilter(query, i)) {
//        println("... filter did NOT pass")
//        return false
//      }
//    }
//    true
//  }
//  
//  /**
//   * Determines whether a filter is relevant by ensuring that
//   * there is at least one variable in `newBindings` present
//   * and that all information for the given filter is available
//   */
//  def isRelevantFilter(query: Array[Int], newBindings: Array[Int], queryNr: Int): Boolean = {
//    val filter = filterList(query.queryId)(queryNr)
//    return allInfoAvailable(filter, query) && containsNewBinding(filter, newBindings) 
//  }
//  
//  /**
//   * Checks that all necessary bindings are present to evaluate
//   * a filter
//   */
//  private def allInfoAvailable(filter: FilterTriple, query: Array[Int]): Boolean = {
//    if (filter.lhsIsVar && query.binding(filter.lhs) == 0) return false
//    if (filter.rhsIsVar && query.binding(filter.rhs) == 0) return false
//    true
//  }
//  
//  /**
//   * Checks that a given filter contains at least one variable
//   * given in `newBindings`
//   */
//  private def containsNewBinding(filter: FilterTriple, newBindings: Array[Int]): Boolean = {
//    if (filter.lhsIsVar && newBindings.contains(-filter.lhs)) return true
//    if (filter.rhsIsVar && newBindings.contains(-filter.rhs)) return true
//    false
//  }
//
//  
//  /**
//   * Processes a query particle for all relevant filters and then
//   * forwards it to its next destination if the filters pass
//   * The query particle has added information at the end:
//   *  [query particle] + [new variables that are bound] + [number of newly bounded variables] 
//   *   + [destination as long in two int fields]
//   */
//  def checkAndForward(query: Array[Int], graphEditor: GraphEditor[Long, Any]) {
//    println(s"DictionaryVertex::checkAndForward query=" + query.mkString(" "))
//    
//    val destination = EfficientIndexPattern.embed2IntsInALong(query(query.size-2), query(query.size-1))
//    val numberOfNewBindings = query(query.length-3)
//    val newBindings = query.takeRight(numberOfNewBindings+3).dropRight(3)
//    
//    if (checkAllFilters(query, newBindings)) {
//      val filterResponse = query.dropRight(newBindings.length + 3)
//      graphEditor.sendSignal(filterResponse, destination)
//      
//      val eip = new EfficientIndexPattern(destination).toTriplePattern
//      println("... Passed filters; sending to " + eip)
//    }
//    else {      
//      val queryVertexId = QueryIds.embedQueryIdInLong(query.queryId)
//      graphEditor.sendSignal(query.tickets, queryVertexId)
//      
//      println("... Did not pass filters")
//    }
//  }
//  
//  /**
//   * Gets the value from the dictionary for a given
//   * binding in the query particle
//   */
//  def varToValue(query: Array[Int], index: Int): Option[String] = {
//    val varValue = if (index > 0) index else query.getBinding(index) // else also includes index=0
//    if (varValue > 0) 
//     Some(d.get(varValue))
//    else None
//  }
//  
//  /**
//   * Gets real values from the dictionary. lhs and rhs might be variables or literal
//   * integers. None is returned if a variable is not bound.
//   */
//  def getRealValues(query: Array[Int], filter: FilterTriple): (Option[String], Option[String]) = {
//    (
//     if(filter.lhsIsVar) varToValue(query, filter.lhs) else None,
//     if(filter.rhsIsVar) varToValue(query, filter.rhs) else None
//    )
//  }
//  
//  /**
//   * Entry point method to test if a filter passes.
//   * This assumes that it has been checked with isRelevantFilter()
//   * beforehand [or, more specifically, with allInfoAvailable()].
//   */
//  def passesFilter(query: Array[Int], filterIndex: Int): Boolean = {
//    val filter = filterList(query.queryId)(filterIndex)
//    val (lhs, rhs) = getRealValues(query, filter)
//    filter.passes(lhs, rhs)
//  }
  
}