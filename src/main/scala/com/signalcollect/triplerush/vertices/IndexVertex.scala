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
import com.signalcollect.triplerush.CardinalityReply
import com.signalcollect.triplerush.CardinalityRequest
import com.signalcollect.triplerush.ChildIdRequest
import com.signalcollect.triplerush.FilterResponse
import com.signalcollect.triplerush.FilterRequest
import com.signalcollect.triplerush.ObjectCountSignal
import com.signalcollect.triplerush.PlaceholderEdge
import com.signalcollect.triplerush.SubjectCountSignal
import com.signalcollect.triplerush.TrGlobal
import com.signalcollect.triplerush.EfficientIndexPattern

/**
 * This vertex represents part of the TripleRush index.
 */
abstract class IndexVertex[State](val id: Long)
  extends BaseVertex[State]
  with ParentBuilding[State] {

  override def expose: Map[String, Any] = {
    val indexType = getClass.getSimpleName
    val d = TrGlobal.dictionary
    val p = new EfficientIndexPattern(id)
    Map[String, Any](
      "Subject" -> d.get(p.s),
      "Predicate" -> d.get(p.p),
      "Object" -> d.get(p.o),
      "TriplePattern" -> s"(SID=${p.s}, PID=${p.p}, OID=${p.o})")
  }

  def foreachChildDelta(f: Int => Unit)

  def addChildDelta(delta: Int): Boolean

  def processQuery(query: Array[Int], graphEditor: GraphEditor[Long, Any])
  
  def checkDictionary(query: Array[Int], graphEditor: GraphEditor[Long, Any])

  def handleCardinalityIncrement(i: Int) = {}

  def handleObjectCount(count: ObjectCountSignal) = {}

  def handleSubjectCount(count: SubjectCountSignal) = {}

  def cardinality: Int

  /**
   * Default reply, is only overridden by SOIndex.
   */
  def handleCardinalityRequest(c: CardinalityRequest, graphEditor: GraphEditor[Long, Any]) {
    println("handleCardinalityRequest: sending " + CardinalityReply(
      c.forPattern, cardinality) + " to " + c.requestor)
    graphEditor.sendSignal(CardinalityReply(
      c.forPattern, cardinality), c.requestor)
  }

  override def addEdge(e: Edge[Long], graphEditor: GraphEditor[Long, Any]): Boolean = {
    val placeholderEdge = e.asInstanceOf[PlaceholderEdge]
    val wasAdded = addChildDelta(placeholderEdge.childDelta)
    wasAdded
  }

  def handleChildIdRequest(requestor: Long, graphEditor: GraphEditor[Long, Any])


  override def deliverSignalWithoutSourceId(signal: Any, graphEditor: GraphEditor[Long, Any]) = {
    signal match {
      case query: Array[Int] =>
        println("\n==== IndexVertex::" + expose.toList(3) + "=====")
        println("Deliver Signal Array[Int]: " + query.mkString(", "))
        checkDictionary(query, graphEditor)
      case filter: FilterRequest =>
        println("\n==== IndexVertex::" + expose.toList(3) + "=====")
        println("Deliver Signal FilterRequest: " + filter.query.mkString(", "))
        processQuery(filter.query, graphEditor)
      case response: FilterResponse =>
        println("\n==== IndexVertex::" + expose.toList(3) + "=====")
        println("Deliver Signal FilterResponse: " + response.query.mkString(", "))
        processQuery(response.query, graphEditor)
      case cr: CardinalityRequest =>
        println("Cardinality Request: " + cr)
        handleCardinalityRequest(cr, graphEditor)
      case ChildIdRequest(requestor) =>
        println("ChildIdRequest: " + requestor)
        handleChildIdRequest(requestor, graphEditor)
      case cardinalityIncrement: Int =>
        println("cardinalityIncrement: " + cardinalityIncrement)
        handleCardinalityIncrement(cardinalityIncrement)
      case count: ObjectCountSignal =>
        println("objectCountSignal: " + count)
        handleObjectCount(count)
      case count: SubjectCountSignal =>
        println("subjectCountSignal: " + count)
        handleSubjectCount(count)
      case other => throw new Exception(s"Unexpected signal @ $id: $other")
    }
    true
  }

}
