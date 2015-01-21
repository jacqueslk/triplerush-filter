package com.signalcollect.triplerush

import org.scalatest.prop.Checkers
import org.scalatest.FlatSpec
import com.signalcollect.triplerush.sparql.Sparql

/**
 * Temporary test case to fiddle around with.
 */
class FilterProblems extends FlatSpec with Checkers { 
  
  implicit val tr = new TripleRush
  
  "SPARQL" should "" in {   
    try {
      tr.addTriple("http://example.org/switzerland", "http://example.org/continent", "http://example.org/europe")
      tr.addTriple("http://example.org/nepal",       "http://example.org/continent", "http://example.org/asia")
      tr.addTriple("http://example.org/switzerland", "http://example.org/population",  "8183800")
      tr.addTriple("http://example.org/nepal",       "http://example.org/population", "26494500")
      
      tr.addTriple("http://example.org/nepal",       "http://example.org/likes", "http://example.org/europe")
      
//            val queryString = """
//   SELECT ?country 
//   WHERE {
//      ?country <http://example.org/population> ?pop .
//      ?pop <http://example.org/populationOf> ?bogus
//      FILTER(?pop > 10000000)
//   }"""
      tr.addTriple("8183800",  "http://example.org/populationOf", "http://ch")
      tr.addTriple("26494500", "http://example.org/populationOf", "http://np")
      
      tr.addTriple("http://example.org/belgium", "http://example.org/continent", "http://example.org/europe")
      tr.addTriple("http://example.org/germany", "http://example.org/continent", "http://example.org/europe")
      tr.addTriple("http://example.org/estonia", "http://example.org/continent", "http://example.org/europe")
      
      println("START PREPARE EXECUTION\r================================")
      tr.prepareExecution
      println("END PREPARE EXECUTION\r================================\r\r\r\r")
      
      val variables = List("country")
      val queryString = """
   SELECT ?country
   WHERE {
      ?country <http://example.org/population> ?pop .
      FILTER(?pop > 10000000)
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
