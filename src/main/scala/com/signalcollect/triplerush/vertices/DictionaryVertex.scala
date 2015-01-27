package com.signalcollect.triplerush.vertices

import scala.collection.mutable.HashMap
import com.signalcollect.GraphEditor
import com.signalcollect.triplerush.Dictionary
import com.signalcollect.triplerush.EfficientIndexPattern
import com.signalcollect.triplerush.EfficientIndexPattern.longToIndexPattern
import com.signalcollect.triplerush.Filter
import com.signalcollect.triplerush.TrGlobal
import com.signalcollect.triplerush.TriplePattern
import com.signalcollect.triplerush.TripleRush
import com.signalcollect.triplerush.QueryIds
import com.signalcollect.triplerush.QueryParticle
import com.signalcollect.triplerush.QueryParticle.arrayToParticle
import com.signalcollect.util.SplayIntSet

// FilterTriple case classes / traits
import com.signalcollect.triplerush.PrimaryExpression
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

final class DictionaryVertex(d: Dictionary) extends IndexVertex(Long.MaxValue) {
  
  val filterList = HashMap.empty[Int, Seq[Filter]];
  
  // Unused methods that must be implemented because of IndexVertex
  override def addChildDelta(delta: Int): Boolean = false
  override def cardinality: Int = 0
  override def foreachChildDelta(f: Int => Unit): Unit = { }
  override def handleChildIdRequest(requestor: Long, graphEditor: GraphEditor[Long, Any]): Unit = { }

  /**
   * Query particles with added meta data are sent to this method
   * by the delivery method in IndexVertex.
   */
  override def processQuery(query: Array[Int], graphEditor: GraphEditor[Long, Any]) {
    checkAndForward(query, graphEditor)
  }
  
  /**
   * Processes a query particle for all relevant filters and then
   * forwards it to its next destination if the filters pass
   * The query particle has added information at the end:
   *  [query particle] + [new variables that are bound] + [number of newly bounded variables] 
   *   + [destination as long in two int fields]
   */
  def checkAndForward(query: Array[Int], graphEditor: GraphEditor[Long, Any]) {
    // Extract meta information at the end of the query particle
    val destination = EfficientIndexPattern.embed2IntsInALong(query(query.size-2), query(query.size-1))
    val numberOfNewBindings = query(query.length-3)
    val newBindings = query.takeRight(numberOfNewBindings+3).dropRight(3)
    
    if (checkAllFilters(query, newBindings)) {
      val filterResponse = query.dropRight(newBindings.length + 3)
      graphEditor.sendSignal(filterResponse, destination)
    }
    else {      
      val queryVertexId = QueryIds.embedQueryIdInLong(query.queryId)
      graphEditor.sendSignal(query.tickets, queryVertexId)
    }
  }
  
  /**
   * Registers or removes a filter list for a given query ID
   */
  override def registerFilters(queryId: Int, queryFilters: Seq[Filter], removal: Boolean) {
    if (removal) {
      filterList -= queryId
    }
    else {
      filterList(queryId) = queryFilters
      summarizeNoVarFilters(queryId)
    }
  }
  
  /**
   * Checks & removes all filters with no variables and adds a
   * "global false" filter at the beginning if the query cannot
   * succeed (i.e. if a no var filter returned false)
   */
  def summarizeNoVarFilters(queryId: Int) {
    val noVarPassed = removeNoVarFilters(queryId)
    if (!noVarPassed) {
      filterList(queryId) = Filter.globalFalse +: filterList(queryId)
    }
  }
  
  /**
   * Returns whether a filter doesn't make use of any variables.
   * This is very inefficient, considering that we could immediately return
   * false once we found one variable, rather than using `getVariableSet()`,
   * which goes through the entire tree to find all variables.
   */
  private def isNoVarFilter(filter: Filter): Boolean = filter.getVariableSet.isEmpty
  
  /**
   * Checks & removes filters with no variables (i.e. filters that
   * can be checked at the beginning) until a no-var filter evaluates
   * to false, implying that the query doesn't have any results
   */
  private def removeNoVarFilters(queryId: Int): Boolean = {
    var i = 0
    filterList(queryId).foreach { filter =>
      if (isNoVarFilter(filter)) {
        if (!filter.passes(Map())) {
          return false
        }
        removeFilterFromList(queryId, i)
        i -= 1 // compensate for filter being removed
      }
      i += 1
    }
    true
  }
  
  /**
   * Helper method to remove an item from a list with a given index
   */
  private def removeFilterFromList(queryId: Int, index: Int) {
    filterList(queryId) = filterList(queryId).take(index) ++ filterList(queryId).drop(index+1)
  }
  
  /**
   * Checks all filters that require information from newly bound variables
   * @param query The query particle
   * @param newBindings A list of variable IDs which have been newly bound
   */
  def checkAllFilters(query: Array[Int], newBindings: Array[Int]): Boolean = {
    if (!filterList.contains(query.queryId)) {
      return true
    } 
    if (filterList(query.queryId).length > 0 && filterList(query.queryId)(0).isGlobalFalse) {
      return false
    }
    var bindingValues = Map[Int, String]()
    for (i <- 0 until filterList(query.queryId).length) {
      val filter = filterList(query.queryId)(i)
      val variableSet = filter.getVariableSet
      if (isRelevantFilter(query, newBindings, variableSet)) {
        bindingValues = addToBindingValues(bindingValues, query, variableSet)
        if (!filter.passes(bindingValues)) {
          return false
        }
      }
    }
    true
  }
  
  
  /**
   * Determines whether a filter is relevant by ensuring that
   * there is at least one variable in `newBindings` present
   * and that all information for the given filter is available
   */
  def isRelevantFilter(query: Array[Int], newBindings: Array[Int], variableSet: Set[Int]): Boolean = {
    return allInfoAvailable(query, variableSet) && containsNewBinding(newBindings, variableSet)
  }
  
  /**
   * Checks that all necessary bindings are present to evaluate
   * a filter
   */
  private def allInfoAvailable(query: Array[Int], variableSet: Set[Int]): Boolean = {
    variableSet.foreach {
      variable => if (query.getBinding(variable) == 0) return false
    }
    true
  }
  
  /**
   * Checks that a given filter contains at least one variable
   * given in `newBindings`
   */
  private def containsNewBinding(newBindings: Array[Int], variableSet: Set[Int]): Boolean = {
    variableSet.intersect(newBindings.toSet).size > 0
  }
  
  /**
   * Returns a map with new dictionary values that were requested,
   * along with any other values present in `existingMap`.
   */
  private def addToBindingValues(
      existingMap: Map[Int, String],
      query: Array[Int],
      required: Set[Int]): Map[Int, String] = {
    val newMap = scala.collection.mutable.Map[Int, String]()
    required.foreach {
      variable => if (!existingMap.get(variable).isDefined) {
        newMap(variable) = varToValue(query, variable).getOrElse("")
      }
    }
    existingMap ++ newMap.toMap
  }
  
  /**
   * Gets the value from the dictionary for a given
   * variable in the query particle
   * @param query The query particle
   * @param index The variable index to get the value for
   */
  private def varToValue(query: Array[Int], index: Int): Option[String] = {
    val varValue = query.getBinding(index)
    if (varValue > 0) {
      try {
        d.decode(varValue)
      } catch {
        case e: IndexOutOfBoundsException =>
          println("No " + varValue + s" in dict")
          None
      }
    }
    else None
  }
  
}