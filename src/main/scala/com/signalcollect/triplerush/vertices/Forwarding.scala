/*
 *  @author Philip Stutz
 *  @author Mihaela Verman
 *  
 *  Copyright 2013 University of Zurich
 *      
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *  
 *         http://www.apache.org/licenses/LICENSE-2.0
 *  
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *  
 */
package com.signalcollect.triplerush.vertices

import com.signalcollect.GraphEditor
import com.signalcollect.triplerush.QueryParticle.arrayToParticle
import com.signalcollect.triplerush.EfficientIndexPattern
import com.signalcollect.triplerush.FilterRequest
import com.signalcollect.triplerush.QueryIds

trait Forwarding[State] extends IndexVertex[State] {
  
  val DICTIONARY_ID = 10

  override def targetIds: Traversable[Long] = {
    new Traversable[Long] {
      def foreach[U](f: Long => U) {
        foreachChildDelta { delta =>
          f(nextRoutingAddress(delta))
        }
      }
    }
  }

  def nextRoutingAddress(childDelta: Int): Long

  override def processQuery(query: Array[Int], graphEditor: GraphEditor[Long, Any]) {
    if (!query.isBindingQuery &&
      query.numberOfPatterns == 1 &&
      query.isSimpleToBind &&
      id != EfficientIndexPattern(0, 0, 0) // Cardinality stats for root node are not accurate.  
      ) {
      // Take a shortcut and don't actually do the forwarding, just send the cardinality.
      // The isSimpleToBind check excludes complicated cases, where a binding might fail.
      val queryVertexId = QueryIds.embedQueryIdInLong(query.queryId)
      println("=== ROOT ===")
      val debugAddr = new EfficientIndexPattern(queryVertexId).toTriplePattern
      println("processQuery: queryVertexId = " + queryVertexId + " (" + debugAddr + ")")
      graphEditor.sendSignal(cardinality, queryVertexId)
      graphEditor.sendSignal(query.tickets, queryVertexId)
    } else {
      val edges = edgeCount
      val totalTickets = query.tickets
      val absoluteValueOfTotalTickets = if (totalTickets < 0) -totalTickets else totalTickets // inlined math.abs
      val avg = absoluteValueOfTotalTickets / edges
      val complete = avg > 0 && totalTickets > 0
      var extras = absoluteValueOfTotalTickets % edges
      val averageTicketQuery = query.copyWithTickets(avg, complete)
      val aboveAverageTicketQuery = query.copyWithTickets(avg + 1, complete)
      //println("processQuery: " + aboveAverageTicketQuery.mkString(", "))
      def sendTo(childDelta: Int) {
        val routingAddress = nextRoutingAddress(childDelta)
        val eip = new EfficientIndexPattern(routingAddress).toTriplePattern
        println("... Routing address: " + eip + " (child delta: " + childDelta + ")")

        if (extras > 0) {
          extras -= 1
          graphEditor.sendSignal(aboveAverageTicketQuery, routingAddress)
        } else if (avg > 0) {
          graphEditor.sendSignal(averageTicketQuery, routingAddress)
        }
      }
      foreachChildDelta(sendTo)
    }
  }
  
  override def checkDictionary(query: Array[Int], graphEditor: GraphEditor[Long, Any]) {
    if (!query.isBindingQuery &&
      query.numberOfPatterns == 1 &&
      query.isSimpleToBind &&
      id != EfficientIndexPattern(0, 0, 0) // Cardinality stats for root node are not accurate.  
      ) {
      // Take a shortcut and don't actually do the forwarding, just send the cardinality.
      // The isSimpleToBind check excludes complicated cases, where a binding might fail.
      val queryVertexId = QueryIds.embedQueryIdInLong(query.queryId)
      println("=== ROOT ===")
      val debugAddr = new EfficientIndexPattern(queryVertexId).toTriplePattern
      println("checkDictionary: queryVertexId = " + queryVertexId + " (" + debugAddr + ")")
      graphEditor.sendSignal(cardinality, queryVertexId)
      graphEditor.sendSignal(query.tickets, queryVertexId)
    } else {
      import com.signalcollect.triplerush.TrGlobal
      val sendToDictionary = TrGlobal.useDict && false // TODO determine when there's a filter check
      
      if (sendToDictionary) {
        val indexInfo = new EfficientIndexPattern(id)
        val queryWithAddr = query :+ indexInfo.extractFirst :+ indexInfo.extractSecond
        //println("checkDictionary: go to dictionary. query = " + queryWithAddr.mkString(", "))
        graphEditor.sendSignal(queryWithAddr, DICTIONARY_ID)
      }
      else {
        //println("checkDictionary: call processQuery; query = " + query.mkString(", "))
        processQuery(query, graphEditor)
      }
    }
  }

}
