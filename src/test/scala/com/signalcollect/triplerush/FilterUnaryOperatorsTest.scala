package com.signalcollect.triplerush

import org.scalatest.prop.Checkers
import org.scalatest.FlatSpec
import com.signalcollect.triplerush.sparql.Sparql

/**
 * Tests the unary operators !, + and -.
 * We avoid any false uses (such as FILTER(-(5>4)) as the
 * SPARQL spec defines that these should evaluate to the
 * error value, which is currently equivalent to Boolean false
 * in TripleRush.
 */
class FilterUnaryOperatorsTest extends FlatSpec with Checkers {
  val debugMode = false
  
  private def prepareTripleRush: TripleRush = {
    val tr = new TripleRush
    tr.addTriple("http://al", "http://nr1",  "9")
    tr.addTriple("http://br", "http://nr1","-16")
    tr.addTriple("http://ch", "http://nr1", "25")
    tr.addTriple("http://de", "http://nr1", "36")
    tr.addTriple("http://ec", "http://nr1", "49")

    tr.addTriple("http://al", "http://nr2", "18")
    tr.addTriple("http://br", "http://nr2", "81")
    tr.addTriple("http://ch", "http://nr2", "31")
    tr.addTriple("http://de", "http://nr2", "36")
    tr.addTriple("http://ec", "http://nr2", "-5")
    tr.prepareExecution
    tr
  }
  
  "Filter trees" should "process + and - operators before primary expressions" in {
    implicit val tr = prepareTripleRush
    try { 
      // + unary operator
      val queryString = """
        SELECT ?A ?B ?C
      	WHERE {
          ?A <http://nr1> ?B .
          ?A <http://nr2> ?C .
          FILTER(+(?B*?B) = +?B*+?B)
          FILTER(5++5 = 10 && +3+(+3) = 6)
        }"""
      
      val query = Sparql(queryString).get
      val result = query.resultIterator.toList

      assert(result.length == 5)
      
      // - unary operator
      val queryString2 = """
        SELECT ?B ?C
      	WHERE {
          ?A <http://nr1> ?B .
          ?A <http://nr2> ?C
          FILTER(5--5 = 10)
          FILTER(-?B > 0 || 0--?C < 0)
        }"""
      
      val query2 = Sparql(queryString2).get
      val result2 = query2.resultIterator.toList
      
      assert(result2.length == 2)
      result2.foreach( e => assert(e("B").toInt < 0 || e("C").toInt < 0) )
    } finally {
      tr.shutdown
    }
  }
  
  it should "process the unary operator !" in {
    implicit val tr = prepareTripleRush
    try {
      val queryString = """
        SELECT ?A ?B ?C
      	WHERE {
          ?A <http://nr1> ?B .
          FILTER(!(?B > ?C))
          ?A <http://nr2> ?C .
        }"""
      
      val query = Sparql(queryString).get
      val result = query.resultIterator.toList

      assert(result.length == 4)
      result.foreach( e => assert(e("B").toInt <= e("C").toInt) )
      
      
      val queryString2 = """
        SELECT ?B ?C
      	WHERE {
          ?A <http://nr1> ?B .
          ?A <http://nr2> ?C .
          FILTER(!(5 < 5) && !(?B <= 0 || ?C <= 0))
        }"""
      
      val query2 = Sparql(queryString2).get
      val result2 = query2.resultIterator.toList
      
      assert(result2.length == 3)
      result2.foreach( e => assert(e("B").toInt > 0 && e("C").toInt > 0) )
      
      // Combination of all... nested use of !
      val queryString3 = """
        SELECT ?B ?C
      	WHERE {
          ?A <http://nr1> ?B .
          ?A <http://nr2> ?C
          FILTER(!(!(!(!(+3--?C < 30)))))
        }"""
      
      val query3 = Sparql(queryString3).get
      val result3 = query3.resultIterator.toList
      
      assert(result3.length == 2)
      result3.foreach( e => assert(e("C").toInt+3 < 30) )
      
    } finally {
      tr.shutdown
    }
  }

}
