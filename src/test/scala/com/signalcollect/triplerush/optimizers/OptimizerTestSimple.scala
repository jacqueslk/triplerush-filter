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
      tr.addTriple("http://b", "http://p", "123")
      tr.addTriple("http://b", "http://p", "14")
      tr.addTriple("http://b", "http://p", "1")

      println("START PREPARE EXECUTION\r================================")
      tr.prepareExecution
      println("END PREPARE EXECUTION\r================================\r\r\r\r")
      

      
      val variables = List("A", "T", "B")
      val queryString = """
        SELECT ?T ?A ?B
      	WHERE {
          <http://a> <http://p> ?A .
          ?A ?T ?B .
          FILTER(?B > 14)
        }"""
      
      val query = Sparql(queryString).get
      val result = query.resultIterator.toList
      println("Total results: " + result.size)
      result.foreach(e => 
        {
          variables.foreach(v => 
            print(s"?$v = " + e(v) + "; ") 
          )
          println
        }
      )
    } finally {
      tr.shutdown
    }
  }

}
