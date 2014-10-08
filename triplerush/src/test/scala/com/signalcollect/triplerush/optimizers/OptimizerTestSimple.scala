package com.signalcollect.triplerush.optimizers

import com.signalcollect.triplerush.TestAnnouncements
import org.scalatest.prop.Checkers
import org.scalatest.FlatSpec
import org.scalatest.ShouldMatchers
import com.signalcollect.triplerush.TripleRush
import com.signalcollect.triplerush.PredicateSelectivity
import com.signalcollect.triplerush.TriplePattern
import com.signalcollect.triplerush.Dictionary
import com.signalcollect.triplerush.PredicateStats
import com.signalcollect.triplerush.sparql.Sparql

class OptimizerTestSimple extends FlatSpec with Checkers with TestAnnouncements {
  "Optimizer" should "handle SPARQL queries" in {
    implicit val tr = new TripleRush
    try {
      tr.addTriple("http://a", "http://p", "http://b")
      tr.addTriple("http://a", "http://p", "http://c")
      tr.addTriple("http://a", "http://p", "http://d")
      tr.addTriple("http://b", "http://p", "http://c")
      tr.addTriple("http://b", "http://p", "http://e")
      tr.addTriple("http://b", "http://p", "http://d")

      println("START PREPARE EXECUTION--------------")
      tr.prepareExecution
      println("!!!!!!!!!!! END PREPARE EXECUTION")
      
      val queryString = """
        SELECT ?T ?A
      	WHERE {
		  <http://a> <http://p> ?A .
		  ?A <http://p> ?T
      }"""
      
      val query = Sparql(queryString).get
      val result = query.resultIterator.toList
    } finally {
      tr.shutdown
    }
  }

}
