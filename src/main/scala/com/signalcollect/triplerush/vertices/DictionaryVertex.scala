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

  def nextRoutingAddress(childDelta: Int): Long = 0
  
  def addChildDelta(delta: Int): Boolean = false
  
  def cardinality: Int = 0
  
  def foreachChildDelta(f: Int => Unit): Unit = { }
  
  def handleChildIdRequest(requestor: Long, graphEditor: GraphEditor[Long, Any]): Unit = {
    
  }

  override def processQuery(query: Array[Int], graphEditor: GraphEditor[Long, Any]) {
    // Nothing to do here
  }
  
  override def checkDictionary(query: Array[Int], graphEditor: GraphEditor[Long, Any]) {
    checkAndForward(query, graphEditor)
  }
  
  override def registerFilters(queryId: Int, filterList: Seq[FilterTriple]) {
    println(s"Registering $filterList to query ID $queryId")
    this.filterList(queryId) = filterList
  }
  
  def removeFilterList(queryId: Int) {
    filterList -= queryId
  }
  
  def checkAllFilters(query: Array[Int]): Boolean = {
    true
//    object AllDone extends Exception { }
//
//    var i = 0
//    var filterResult = true
//    try {
//      for (i <- 0 until query.numberOfFilters)
//        if (!passesFilter(query, i)) {
//         filterResult = false;
//         throw AllDone
//        }
//    } catch {
//      case AllDone => // break equivalent in Scala
//    }
//    
//    filterResult    
  }
  
  def checkAndForward(query: Array[Int], graphEditor: GraphEditor[Long, Any]) {
    println(">> This is the Dictionary Vertex <<")
    

    
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
  
  def arithmeticFilter(lhs: Int, comparator: String, rhs: Int): Boolean = {
    println(s"DictionaryVertex::arithmeticFilter: $lhs $comparator $rhs")
    comparator match {
      case "="   =>  lhs == rhs
      case ">"   =>  lhs >  rhs
      case "<"   =>  lhs <  rhs
      case "!="  =>  lhs != rhs
      case ">="  =>  lhs >= rhs
      case "<="  =>  lhs <= rhs
      case _     => throw new Exception(s"Unknown operator $comparator")
    }
  }
  
}