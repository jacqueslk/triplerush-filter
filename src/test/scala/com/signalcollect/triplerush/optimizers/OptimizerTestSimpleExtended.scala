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

class OptimizerTestSimpleExtended extends FlatSpec with Checkers with TestAnnouncements {
  "Optimizer" should "handle SPARQL queries" in {
    implicit val tr = new TripleRush
    try {
      tr.addTriple("http://A", "http://p", "http://B")
      tr.addTriple("http://C", "http://p", "http://B")
      tr.addTriple("http://D", "http://p", "http://F")
      tr.addTriple("http://E", "http://p", "http://B")
      tr.addTriple("http://F", "http://p", "http://D")
      tr.addTriple("http://G", "http://p", "http://B")
      tr.addTriple("http://G", "http://p", "http://F")
      tr.addTriple("http://G", "http://p", "http://H")
      tr.addTriple("http://H", "http://p", "http://A")
      tr.addTriple("http://I", "http://p", "http://A")
      
      tr.addTriple("http://A", "http://q", "http://I")
      tr.addTriple("http://B", "http://q", "http://E")
      tr.addTriple("http://B", "http://q", "http://G")
      tr.addTriple("http://C", "http://q", "http://D")
      tr.addTriple("http://D", "http://q", "http://E")
      tr.addTriple("http://E", "http://q", "http://C")
      tr.addTriple("http://F", "http://q", "http://G")
      tr.addTriple("http://G", "http://q", "http://F")
      tr.addTriple("http://G", "http://q", "http://H")
      tr.addTriple("http://H", "http://q", "http://G")
      tr.addTriple("http://H", "http://q", "http://I")
      tr.addTriple("http://I", "http://q", "http://A")
      
      tr.addTriple("http://A", "http://r", "http://D")
      tr.addTriple("http://A", "http://r", "http://G")
      tr.addTriple("http://B", "http://r", "http://F")
      tr.addTriple("http://E", "http://r", "http://B")
      tr.addTriple("http://E", "http://r", "http://G")
      tr.addTriple("http://H", "http://r", "http://F")

      println("START PREPARE EXECUTION--------------")
      tr.prepareExecution
      println("!!!!!!!!!!! END PREPARE EXECUTION\r================================\r\r\r\r")
      
      val variables = List("A", "B", "C")
      val queryString = """
        SELECT ?A ?B ?C
      	WHERE {
		  <http://G> <http://p> ?B .
		  ?B ?A ?C
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
