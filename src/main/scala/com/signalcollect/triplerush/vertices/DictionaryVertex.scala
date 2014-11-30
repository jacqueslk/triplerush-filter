package com.signalcollect.triplerush.vertices

import scala.collection.mutable.HashMap
import com.signalcollect.GraphEditor
import com.signalcollect.triplerush.FilterResponse
import com.signalcollect.triplerush.FilterRequest
import com.signalcollect.triplerush.FilterTriple
import com.signalcollect.triplerush.EfficientIndexPattern
import com.signalcollect.triplerush.EfficientIndexPattern.longToIndexPattern
import com.signalcollect.triplerush.TrGlobal
import com.signalcollect.triplerush.TriplePattern
import com.signalcollect.triplerush.QueryIds
import com.signalcollect.triplerush.QueryParticle
import com.signalcollect.triplerush.QueryParticle.arrayToParticle
import com.signalcollect.util.SplayIntSet

final class DictionaryVertex extends IndexVertex(Long.MaxValue) {
  
  val d = TrGlobal.dictionary
  
  val filterList = HashMap.empty[Int, Seq[FilterTriple]];
  
  // Unused methods that must be implemented because of IndexVertex
  override def addChildDelta(delta: Int): Boolean = false
  override def cardinality: Int = 0
  override def foreachChildDelta(f: Int => Unit): Unit = { }
  override def handleChildIdRequest(requestor: Long, graphEditor: GraphEditor[Long, Any]): Unit = { }

  override def processQuery(query: Array[Int], graphEditor: GraphEditor[Long, Any]) {
    checkAndForward(query, graphEditor)
  }
  
  override def registerFilters(queryId: Int, queryFilters: Seq[FilterTriple]) {
    println(s"DV::registerFilters: Registering $queryFilters to query ID $queryId")
    filterList(queryId) = queryFilters
    summarizeNoVarFilters(queryId)
  }
  
  /**
   * Checks & removes all filters with no variables and adds a
   * "global false" filter at the beginning if the query can
   * not succeed (i.e. if a no var filter returned false)
   */
  def summarizeNoVarFilters(queryId: Int) {
    val noVarPassed = removeNoVarFilters(queryId)
    if (!noVarPassed) {
      filterList(queryId) = FilterTriple.globalFalse +: filterList(queryId)
      println(filterList(queryId))
    }
  }
  
  /**
   * Returns whether a filter doesn't make use of any variables
   */
  private def isNoVarFilter(filter: FilterTriple): Boolean = (!filter.lhsIsVar && !filter.rhsIsVar)
  
  /**
   * Checks & removes filters with no variables (i.e. filters that
   * can be checked at the beginning) and return result
   */
  private def removeNoVarFilters(queryId: Int): Boolean = {
    var i = 0
    var result = true
    filterList(queryId).foreach { e =>
      if (isNoVarFilter(e)) {
        if (result && !e.passes(None, None)) {
          println(s"Filter $e did not pass!!!")
          result = false
        }
        removeFilterFromList(queryId, i)
        i -= 1
      }
      i += 1
    }
    result
  }
  
  /**
   * Helper method to remove an item from a list with a given index
   */
  private def removeFilterFromList(queryId: Int, index: Int) {
    filterList(queryId) = filterList(queryId).take(index) ++ filterList(queryId).drop(index+1)
  }
  
  def removeFilters(queryId: Int) {
    filterList -= queryId
  }
  
  /**
   * Check all possible filters with the given information.
   */
  def checkAllFilters(query: Array[Int]): Boolean = {
    var i = 0
    for (i <- 0 until filterList(query.queryId).length) {
      if (!passesFilter(query, i)) {
       return false
      }
    }
    true
  }
  
  def checkAndForward(query: Array[Int], graphEditor: GraphEditor[Long, Any]) {
    
    if (checkAllFilters(query)) {
      val destination = EfficientIndexPattern.embed2IntsInALong(query(query.size-2), query(query.size-1))
      val filterResponse = FilterResponse(query.dropRight(2))
      
      val eip = new EfficientIndexPattern(destination).toTriplePattern
      println("Passed filter; sending to " + destination + " (= " + eip + ")")
      
      graphEditor.sendSignal(filterResponse, destination) 
    }
    else {
      println("Did not pass filters...")
      val queryVertexId = QueryIds.embedQueryIdInLong(query.queryId)
      print("Sending back " + query.tickets + " to query vertex ID " + queryVertexId)
      println(" (=" + new EfficientIndexPattern(queryVertexId).toTriplePattern + ")")
      graphEditor.sendSignal(query.tickets, queryVertexId) // Send back tickets to query particle
    }
  }
  
  def varToValue(query: Array[Int], index: Int): Option[Int] = {
    val varValue = if (index > 0) index else query.getVariable(index) // else also includes index=0
    if (varValue > 0) 
     None//Some(d.get(varValue).toInt)
    else None
  }
  
  /**
   * Get real values from the dictionary. lhs and rhs might be variables or literal
   * integers. None is returned if a variable is not bound.
   */
  def getRealValues(query: Array[Int], filter: FilterTriple): (Option[Int], Option[Int]) = {
    (
     if(filter.lhsIsVar) varToValue(query, filter.lhs) else Some(filter.lhs),
     if(filter.rhsIsVar) varToValue(query, filter.rhs) else Some(filter.rhs)
    )
  }
  
  def passesFilter(query: Array[Int], filterIndex: Int): Boolean = {
    true
//    val filter = query.filter(filterIndex)
//    val rawComparator = filter.comparatorNoFlags
//    if (0 < rawComparator && rawComparator <= 6) {
//      val (lhsVal, rhsVal) = getRealValues(query, filter)
//      if (lhsVal.isDefined && rhsVal.isDefined) {
//        return arithmeticFilter(lhsVal.get, filter.intToOperator, rhsVal.get)
//      }
//    }
//    return true
  }
  
}