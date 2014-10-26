package com.signalcollect.triplerush.vertices

import scala.util.Random
import com.signalcollect.GraphEditor
import com.signalcollect.triplerush.FilterResponse
import com.signalcollect.triplerush.FilterRequest
import com.signalcollect.triplerush.EfficientIndexPattern
import com.signalcollect.triplerush.EfficientIndexPattern.longToIndexPattern
import com.signalcollect.triplerush.TriplePattern
import com.signalcollect.triplerush.QueryIds
import com.signalcollect.triplerush.QueryParticle
import com.signalcollect.triplerush.QueryParticle.arrayToParticle
import com.signalcollect.util.SplayIntSet

final class DictionaryVertex extends IndexVertex(Long.MaxValue) {
  
  val random = new Random();

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
  
  def checkAndForward(query: Array[Int], graphEditor: GraphEditor[Long, Any]) {
    println(">> This is the Dictionary Vertex <<")
    
    if (passesFilter) {
      val destination = EfficientIndexPattern.embed2IntsInALong(query(query.size-2), query(query.size-1))
      val filterResponse = FilterResponse(query.dropRight(2))
      
      val eip = new EfficientIndexPattern(destination).toTriplePattern
      println("Passed filter; sending to " + destination + " (= " + eip + ")")
      
      graphEditor.sendSignal(filterResponse, destination) 
    }
    else {
      println("Did not pass filter...")
      val queryVertexId = QueryIds.embedQueryIdInLong(query.queryId)
      print("Sending back " + query.tickets + " to query vertex ID " + queryVertexId)
      println(" (=" + new EfficientIndexPattern(queryVertexId).toTriplePattern + ")")
      graphEditor.sendSignal(query.tickets, queryVertexId) // Send back tickets to query particle
    }
  }
  
  def passesFilter: Boolean = (random.nextInt() % 6) != 0
  
}