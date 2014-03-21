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

package com.signalcollect.triplerush

import org.scalatest.FlatSpec
import com.signalcollect.GraphBuilder
import com.signalcollect.triplerush.optimizers.PredicateSelectivityEdgeCountsOptimizer
import com.signalcollect.triplerush.optimizers.NoOptimizerCreator

class QuerySpecificationSpec extends FlatSpec with TestAnnouncements {

  "QuerySpecification" should "correctly translate a SPARQL query that has results" in {
    val tr = new TripleRush
    try {
      tr.addTriple("http://a", "http://b", "http://c")
      tr.addTriple("http://a", "http://d", "http://e")
      tr.prepareExecution
      val sparql = """
SELECT ?X
WHERE {
    	?X <http://b> <http://c> .
    	?X <http://d> <http://e>
}
"""
      val queryOption = QuerySpecification.fromSparql(sparql)
      assert(queryOption.isDefined)
      val query = queryOption.get
      println(query)
      val encodedResults = tr.executeQuery(query)
      println("Encoded done")
      val decodedResults = query.decodeResults(encodedResults)
      assert(decodedResults.get.toSet === Set(Map("X" -> "http://a")))
    } catch {
      case t: Throwable =>
        println(t.getMessage)
        t.printStackTrace
        throw t
    } finally {
      tr.shutdown
    }
  }

  it should "correctly translate a SPARQL query that has no results" in {
    val tr = new TripleRush
    try {
      tr.addTriple("http://a", "http://b", "http://c")
      tr.addTriple("http://f", "http://d", "http://e")
      tr.prepareExecution
      val sparql = """
SELECT ?X
WHERE {
    	?X <http://b> <http://c> .
    	?X <http://d> <http://e>
}
"""
      val queryOption = QuerySpecification.fromSparql(sparql)
      assert(queryOption.isDefined)
      val query = queryOption.get
      val encodedResults = tr.executeQuery(query)
      val decodedResults = query.decodeResults(encodedResults)
      assert(decodedResults.get.toSet === Set())
    } catch {
      case t: Throwable =>
        println(t.getMessage)
        t.printStackTrace
        throw t
    } finally {
      tr.shutdown
    }
  }

  it should "correctly eliminate a SPARQL query that is guaranteed to have no results" in {
    val tr = new TripleRush
    try {
      tr.addTriple("http://a", "http://b", "http://c")
      tr.addTriple("http://f", "http://d", "http://e")
      tr.prepareExecution
      val sparql = """
SELECT ?X
WHERE {
    	?X <http://z> <http://c> .
    	?X <http://d> <http://e>
}
"""
      val queryOption = QuerySpecification.fromSparql(sparql)
      assert(queryOption.isEmpty)
    } finally {
      tr.shutdown
    }
  }

}
