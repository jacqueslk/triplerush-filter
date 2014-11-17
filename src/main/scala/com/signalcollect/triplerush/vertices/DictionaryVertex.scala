package com.signalcollect.triplerush.vertices

import scala.util.Random
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
  
  val random = new Random();
  
  val d = TrGlobal.dictionary

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
  
  def checkAllFilters(query: Array[Int]): Boolean = {
    object AllDone extends Exception { }

    var i = 0
    var filterResult = true
    try {
      for (i <- 0 until query.numberOfFilters)
        if (!passesFilter(query, i)) {
         filterResult = false;
         throw AllDone
        }
    } catch {
      case AllDone => // break equivalent in Scala
    }
    
    filterResult    
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
  
  def varToValue(query: Array[Int], index: Int): Option[String] = {
    val varValue = if (index > 0) index else query.getVariable(index) // else also includes index=0
    if (varValue > 0) 
     Some(d.get(varValue))
    else None
  }
  
  def passesFilter(query: Array[Int], filterIndex: Int): Boolean = {
    // Assumption for right now is that lhs is a var
    // and rhs is a constant int
    val filter = query.filter(filterIndex)
    if (0 < filter.comparator && filter.comparator <= 6) {
      val rhsValue = varToValue(query, filter.entry)
      if (rhsValue.isDefined) {
        val result = arithmeticFilter(rhsValue.get.toInt, filter.intToOperator, filter.value)
        if (result) query.removeFilter(filterIndex)
        else println("DID NOT PASS") // < TODO remove
        return result
      }
      else {
        return true
      }
    }
    return true
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