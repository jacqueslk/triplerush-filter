package com.signalcollect.triplerush

import org.scalatest.prop.Checkers
import org.scalatest.FlatSpec
import com.signalcollect.triplerush.sparql.Sparql

/**
 * Temporary test case to fiddle around with.
 */
class FilterTempDebug extends FlatSpec with Checkers { 
  
  implicit val tr = new TripleRush
  
  "SPARQL" should "" in {   
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
      
      val variables = List("simProperty2", "T", "origProperty2")
      val queryString = """
        SELECT ?simProperty2 ?T ?origProperty2
      	WHERE {
          <http://a> <http://p> ?simProperty2 .
          ?simProperty2 ?T ?origProperty2

          FILTER((3+6)*(4+5) = (81/9)*(54-45))
          FILTER((?origProperty2 = ?origProperty2 || 0 > 10) && !(0))
        }"""
      // Unsupported: FILTER (<http://www4.wiwiss.fu-berlin.de/bizer/bsbm/v01/instances/dataFromProducer1/Product35> != ?origProperty2)
      // FILTER(5/?B + ?B*3 - 3 > ?B || ?B*5 != 14)
        // FILTER(5*5 != 14 && 3 > 1)
      
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
