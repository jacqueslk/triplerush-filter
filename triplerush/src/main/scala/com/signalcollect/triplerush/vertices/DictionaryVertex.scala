package com.signalcollect.triplerush.vertices

import scala.util.Random
import com.signalcollect.GraphEditor
import com.signalcollect.triplerush.FilterResponse
import com.signalcollect.triplerush.FilterRequest
import com.signalcollect.triplerush.EfficientIndexPattern
import com.signalcollect.triplerush.EfficientIndexPattern.longToIndexPattern
import com.signalcollect.triplerush.TriplePattern
import com.signalcollect.util.SplayIntSet

final class DictionaryVertex extends IndexVertex(10) {
  
  val random = new Random();

  def nextRoutingAddress(childDelta: Int): Long = 0
  
  def addChildDelta(delta: Int): Boolean = false
  
  def cardinality: Int = 0
  
  def foreachChildDelta(f: Int => Unit): Unit = { }
  
  def handleChildIdRequest(requestor: Long, graphEditor: GraphEditor[Long, Any]): Unit = {
    
  }

  override def processQuery(query: Array[Int], graphEditor: GraphEditor[Long, Any]) {
    checkAndForward(query, graphEditor)
  }
  
  override def checkDictionary(query: Array[Int], graphEditor: GraphEditor[Long, Any]) {
    // Nothing here.
  }
  
  def checkAndForward(query: Array[Int], graphEditor: GraphEditor[Long, Any]) {
    println("<<<<< Dictionary Vertex >>>>>")
    println("Query: " + query.mkString(", "))
    
    if (checkFilter) {
      val s = query(query.size-3)
      val p = query(query.size-2)
      val o = query(query.size-1)
      val destination = TriplePattern(s, p, o).routingAddress
      val filterResponse = FilterResponse(query)
      
      val eip = new EfficientIndexPattern(destination).toTriplePattern // DEBUG INFO
      println("Passed filter; sending to " + destination + "(= " + eip + ")")
      
      graphEditor.sendSignal(filterResponse, destination) 
    }
    else {
      println("Did not pass filter...")
      graphEditor.sendSignal(query, 0) // Send back to query ??
    }
  }
  
  def checkFilter: Boolean = true //(random.nextInt() & 1) == 0
  
}