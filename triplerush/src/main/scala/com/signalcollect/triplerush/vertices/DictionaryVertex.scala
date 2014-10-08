package com.signalcollect.triplerush.vertices

import scala.util.Random
import com.signalcollect.GraphEditor
import com.signalcollect.triplerush.FilterRequest
import com.signalcollect.triplerush.EfficientIndexPattern
import com.signalcollect.triplerush.EfficientIndexPattern.longToIndexPattern
import com.signalcollect.util.SplayIntSet

final class DictionaryVertex extends OptimizedIndexVertex(Long.MaxValue) {
  //with Forwarding[Any] {
  
  val random = new Random();

  def nextRoutingAddress(childDelta: Int): Long = 0
  
  def deliverSignalWithoutSourceId(signal: FilterRequest, graphEditor: GraphEditor[Long, Any]) : Boolean = {
    println("GOT FILterREQUEST")
    if (checkFilter) {
      
    }
    else {
      graphEditor.sendSignal(signal, EfficientIndexPattern(0, 0, 0))
    }
    
    return true
  }
  
  override def processQuery(query: Array[Int], graphEditor: GraphEditor[Long, Any]) = {
    
  }
  
  def checkFilter: Boolean = {
    if ((random.nextInt() & 1) == 0) {
      true
    }
    else {
      false
    }
  }
  
}