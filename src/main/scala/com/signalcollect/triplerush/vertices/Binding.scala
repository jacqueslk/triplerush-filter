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

import com.signalcollect.Edge
import com.signalcollect.GraphEditor
import com.signalcollect.triplerush.EfficientIndexPattern.longToIndexPattern
import com.signalcollect.triplerush.FilterPending
import com.signalcollect.triplerush.QueryParticle.arrayToParticle
import com.signalcollect.triplerush.QueryIds

trait Binding
  extends IndexVertex[Any]
  with ParentBuilding[Any] {

  def onEdgeAdded(ge: GraphEditor[Long, Any])

  def incrementParentIndexCardinalities(ge: GraphEditor[Long, Any]) {
    for (parent <- id.parentIds) {
      ge.sendSignal(1, parent)
    }
  }

  def decrementParentIndexCardinalities(ge: GraphEditor[Long, Any]) {
    for (parent <- id.parentIds) {
      ge.sendSignal(-1, parent)
    }
  }

  override def addEdge(e: Edge[Long], ge: GraphEditor[Long, Any]): Boolean = {
    val wasAdded = super.addEdge(e, ge)
    if (wasAdded) {
      onEdgeAdded(ge)
    }
    wasAdded
  }

  def bindIndividualQuery(childDelta: Int, queryParticle: Array[Int]): Array[Int]

  def processQuery(query: Array[Int], graphEditor: GraphEditor[Long, Any]) {
    bindQueryToAllTriples(query, graphEditor)
  }

  def bindQueryToAllTriples(query: Array[Int], graphEditor: GraphEditor[Long, Any]) {
    if (!query.isBindingQuery &&
      query.numberOfPatterns == 1 &&
      query.isSimpleToBind) {
      // Take a shortcut and don't actually do the binding, just send the result count.
      // The isSimpleToBind check excludes complicated cases, where a binding might fail.
      val queryVertexId = QueryIds.embedQueryIdInLong(query.queryId)
      graphEditor.sendSignal(edgeCount, queryVertexId)
      graphEditor.sendSignal(query.tickets, queryVertexId)
    } else {
      val edges = edgeCount
      val totalTickets = query.tickets
      val absoluteValueOfTotalTickets = if (totalTickets < 0) -totalTickets else totalTickets  // inlined math.abs
      val avg = absoluteValueOfTotalTickets / edges
      val complete = avg > 0 && totalTickets > 0
      var extras = absoluteValueOfTotalTickets % edges
      val averageTicketQuery = query.copyWithTickets(avg, complete)
      val aboveAverageTicketQuery = query.copyWithTickets(avg + 1, complete)
      def bind(childDelta: Int) {
        if (extras > 0) {
          extras -= 1
          handleQueryBinding(childDelta, aboveAverageTicketQuery, graphEditor)
        } else if (avg > 0) {
          handleQueryBinding(childDelta, averageTicketQuery, graphEditor)
        }
      }
      foreachChildDelta(bind)
    }
  }

  def handleQueryBinding(
    childDelta: Int,
    query: Array[Int],
    graphEditor: GraphEditor[Long, Any]) {
    val boundParticle = bindIndividualQuery(childDelta, query)
    if (boundParticle != null) {
      // Potential for improvement: Note that findNewBindings() is called
      // for every copy of the query particle, rather than once before any
      // binding action takes place. We are only interested in the variable
      // IDs that get bound.
      val newBindings = findNewBindings(query, boundParticle)
      routeSuccessfullyBound(boundParticle, newBindings, graphEditor)
    } else {
      // Failed to bind, send to query vertex.
      val queryVertexId = QueryIds.embedQueryIdInLong(query.queryId)
      graphEditor.sendSignal(query.tickets, queryVertexId)
    }
  }
  
  def findNewBindings(oldParticle: Array[Int], newParticle: Array[Int]): Seq[Int] = {
    (-1 to -oldParticle.numberOfBindings by -1) filter(i => (
        oldParticle.getBinding(i) != newParticle.getBinding(i))
    )
  }

  def routeSuccessfullyBound(
    boundParticle: Array[Int],
    newBindings: Seq[Int],
    graphEditor: GraphEditor[Long, Any]) {

    if (boundParticle.isResult && !boundParticle.isBindingQuery) {
      val queryVertexId = QueryIds.embedQueryIdInLong(boundParticle.queryId)
      graphEditor.sendSignal(1, queryVertexId)
      graphEditor.sendSignal(boundParticle.tickets, queryVertexId)
    }
    else {
      val destination = if(boundParticle.isResult) QueryIds.embedQueryIdInLong(boundParticle.queryId)
                                              else boundParticle.routingAddress
      if (newBindings.length == 0 || !boundParticle.isBindingQuery) {
        graphEditor.sendSignal(boundParticle, destination)
      }
      else {
        // Need to consult dictionary vertex -- add meta info (new bindings + destination
        // after filter check) to the end of the query particle
        import Array.concat
        val queryWithMetaInfo = concat(boundParticle, newBindings.toArray) :+ newBindings.length :+ destination.extractFirst :+ destination.extractSecond
        
        if (boundParticle.isResult) {
          // Send query particle to dictionary vertex immediately if it is destined for the
          // query vertex; we know it exists + we avoid modifying query vertex code 
          graphEditor.sendSignal(queryWithMetaInfo, DICTIONARY_ID)
        }
        else {
          // Not going to query vertex: send to destination first to ensure that the next
          // destination exists. This way, no filters are checked for inexistent vertices.
          graphEditor.sendSignal(new FilterPending(queryWithMetaInfo), destination)
        }
      }
    }
  }

}