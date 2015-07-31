/*
 *  @author Philip Stutz
 *  
 *  Copyright 2014 University of Zurich
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

package com.signalcollect.triplerush.handlers

import com.signalcollect.{ Edge, Vertex }
import com.signalcollect.interfaces.{ EdgeAddedToNonExistentVertexHandler, EdgeAddedToNonExistentVertexHandlerFactory }
import com.signalcollect.triplerush.EfficientIndexPattern.longToIndexPattern
import com.signalcollect.triplerush.TriplePattern
import com.signalcollect.triplerush.vertices.{ OIndex, PIndex, POIndex, RootIndex, SIndex, SOIndex, SPIndex }
import com.signalcollect.GraphEditor
import com.signalcollect.triplerush.IndexVertexEdge
import com.signalcollect.triplerush.EfficientIndexPattern.longToIndexPattern

case object TripleRushEdgeAddedToNonExistentVertexHandlerFactory extends EdgeAddedToNonExistentVertexHandlerFactory[Long, Any] {
  def createInstance: EdgeAddedToNonExistentVertexHandler[Long, Any] = TripleRushEdgeAddedToNonExistentVertexHandler
  override def toString = "TripleRushEdgeAddedToNonExistentVertexHandlerFactory"
}

case object TripleRushEdgeAddedToNonExistentVertexHandler extends EdgeAddedToNonExistentVertexHandler[Long, Any] {
  def handleImpossibleEdgeAddition(edge: Edge[Long], vertexId: Long, graphEditor: GraphEditor[Long, Any]): Option[Vertex[Long, _, Long, Any]] = {
    vertexId.parentIds foreach { parentId =>
      val idDelta = vertexId.parentIdDelta(parentId)
      graphEditor.addEdge(parentId, new IndexVertexEdge(idDelta))
    }
    val s = vertexId.s
    val p = vertexId.p
    val o = vertexId.o
    val triplePattern = TriplePattern(s, p, o)
    triplePattern match {
      case TriplePattern(0, 0, 0) => Some(new RootIndex)
      case TriplePattern(0, 0, o) => Some(new OIndex(vertexId))
      case TriplePattern(0, p, 0) => Some(new PIndex(vertexId))
      case TriplePattern(s, 0, 0) => Some(new SIndex(vertexId))
      case TriplePattern(s, p, 0) => Some(new SPIndex(vertexId))
      case TriplePattern(s, 0, o) => Some(new SOIndex(vertexId))
      case TriplePattern(0, p, o) => Some(new POIndex(vertexId))
      case other                  => throw new Exception(s"Could not add edge $edge to vertex $triplePattern, because that vertex does not exist.")
    }
  }
}
