package com.signalcollect.triplerush

import org.scalatest.prop.Checkers
import org.scalatest.FlatSpec
import com.signalcollect.triplerush.sparql.Sparql

/**
 * Tests the arithmetic capabilities of the SPARQL filter,
 * such as division or subtraction with or without variables.
 * The SPARQL queries also include various combinations of
 * optional periods (.) between filter triple and FILTER
 * to test the parser rule.
 */
class FilterArithmeticMathTest extends FlatSpec with Checkers {
  val debugMode = false
  
  private def prepareTripleRush: TripleRush = {
    val tr = new TripleRush
    tr.addTriple("http://al", "http://nr1",  "9")
    tr.addTriple("http://be", "http://nr1","-16")
    tr.addTriple("http://ch", "http://nr1", "25")
    tr.addTriple("http://de", "http://nr1", "36")
    tr.addTriple("http://ec", "http://nr1", "49")

    tr.addTriple("http://al", "http://nr2", "18")
    tr.addTriple("http://be", "http://nr2", "81")
    tr.addTriple("http://ch", "http://nr2", "31")
    tr.addTriple("http://de", "http://nr2", "36")
    tr.addTriple("http://ec", "http://nr2", "-5")
    tr.prepareExecution
    tr
  }
  
  "Arithmetic filters" should "process addition and subtraction properly" in {
    implicit val tr = prepareTripleRush
    try {
      // Simple addition check with <= 
      val queryString = """
        SELECT ?A ?B ?C
      	WHERE {
          ?A <http://nr1> ?B .
          ?A <http://nr2> ?C .
          FILTER(?B+?B <= ?C)
        }"""
      
      val query = Sparql(queryString).get
      val result = query.resultIterator.toList

      assert(result.length == 2)
      result.foreach( e => assert(e("B").toInt*2 <= e("C").toInt) )
      
      // 0-?var with OR operator
      val queryString2 = """
        SELECT ?B ?C
      	WHERE {
          ?A <http://nr1> ?B .
          ?A <http://nr2> ?C
          FILTER(0 - ?B>0 || 0-?C > 0)
        }"""
      
      val query2 = Sparql(queryString2).get
      val result2 = query2.resultIterator.toList
      
      assert(result2.length == 2)
      result2.foreach( e => assert(e("B").toInt < 0 || e("C").toInt < 0) )
      
      // addition with AND operator
      val queryString3 = """
        SELECT ?A ?B ?C
      	WHERE {
          ?A <http://nr1> ?B
          FILTER(?B + ?C > 20 && ?B < 40 && ?C >= 20)
          ?A <http://nr2> ?C .
        }"""
      
      val query3 = Sparql(queryString3).get
      val result3 = query3.resultIterator.toList
      
      assert(result3.length == 3)
      result3.foreach( e => assert(
                    e("B").toInt + e("C").toInt > 20
                 && e("B").toInt < 40
                 && e("C").toInt >= 20
      ))
      
      // multiple elements in addition, negative modifier
      val queryString4 = """
        SELECT ?A ?B ?C
      	WHERE {
          FILTER(-?B + -?B + ?C < 4+3-7+1)
          ?A <http://nr1> ?B .
          ?A <http://nr2> ?C
        }"""
      
      val query4 = Sparql(queryString4).get
      val result4 = query4.resultIterator.toList
      
      assert(result4.length == 4)
      result4.foreach( e => assert(0-e("B").toInt*2 + e("C").toInt < 1) )
      
    } finally {
      tr.shutdown
    }
  }
  
  it should "process multiplication properly" in {
    implicit val tr = prepareTripleRush
    try {
      // Simple multiplication
      val queryString = """
        SELECT ?A ?B ?C
      	WHERE {
          ?A <http://nr1> ?B .
          FILTER(?B*2 > ?C)
          ?A <http://nr2> ?C .
        }"""
      
      val query = Sparql(queryString).get
      val result = query.resultIterator.toList

      assert(result.length == 3)
      result.foreach( e => assert(e("B").toInt*2 > e("C").toInt) )
      
      // Division with OR operator
      val queryString2 = """
        SELECT ?B ?C
      	WHERE {
          ?A <http://nr1> ?B .
          ?A <http://nr2> ?C .
          FILTER(?B/?C > 0.7 || 1000 / ?C*?B < -500)
        }"""
      
      val query2 = Sparql(queryString2).get
      val result2 = query2.resultIterator.toList
      
      assert(result2.length == 3)
      result2.foreach( e => assert(
             e("B").toDouble/e("C").toDouble > 0.7
          || 1000/e("C").toDouble*e("B").toDouble < -500
      ))
      
      // AND operator with division/multiplication
      val queryString3 = """
        SELECT ?B ?C
      	WHERE {
          ?A <http://nr1> ?B .
          ?A <http://nr2> ?C
          FILTER(?B*4/2*2 >= 100 && ?C*?B > 700/7/4/25 && ?B/10+?C/35 > 3)
        }"""
      
      val query3 = Sparql(queryString3).get
      val result3 = query3.resultIterator.toList
      
      assert(result3.length == 2)
      result3.foreach( e => assert(
             e("B") == "25" && e("C") == "31"
          || e("B") == "36" && e("C") == "36"
      ))
      
      // global filters testing precedence of operators
      val queryString4 = """
        SELECT ?A ?B ?C
      	WHERE {
          ?A <http://nr1> ?B .
          FILTER(3*5 = 15 && 49/7 = 3+4)
          FILTER(1+2-3*4/2 = -3)
          FILTER(12/4*3-6+2 = 5 || 3*4 < 1)
          ?A <http://nr2> ?C
        }"""
      
      val query4 = Sparql(queryString4).get
      val result4 = query4.resultIterator.toList
      
      assert(result4.length == 5)
      
    } finally {
      tr.shutdown
    }
  }
  
  it should "handle bracketed expressions" in {
    implicit val tr = prepareTripleRush
    try {
      // Simple bracket expression
      val queryString = """
        SELECT ?A ?B ?C
      	WHERE {
          ?A <http://nr1> ?B .
          ?A <http://nr2> ?C
          FILTER((?B+?C)/10 <= 5)
        }"""
      
      val query = Sparql(queryString).get
      val result = query.resultIterator.toList

      assert(result.length == 2)
      result.foreach( e => assert((e("B").toInt + e("C").toInt)/10 <= 5) )
      
      // Boolean expression in brackets
      val queryString2 = """
        SELECT ?B ?C
      	WHERE {
          ?A <http://nr2> ?C
          FILTER((?B < 30))
          ?A <http://nr1> ?B
        }"""
      
      val query2 = Sparql(queryString2).get
      val result2 = query2.resultIterator.toList
      
      assert(result2.length == 3)
      result2.foreach( e => assert(e("B").toInt < 30) )
      
      // Mix of arithmetic and boolean brackets
      val queryString3 = """
        SELECT ?B ?C
      	WHERE {
          FILTER((?B-10)/2 > 0 && (?C = 31 || ?C < 0))
          ?A <http://nr1> ?B .
          ?A <http://nr2> ?C
        }"""
      
      val query3 = Sparql(queryString3).get
      val result3 = query3.resultIterator.toList
      
      assert(result3.length == 2)
      result3.foreach( e => assert(
             e("B") == "25" && e("C") == "31"
          || e("B") == "49" && e("C") == "-5"
      ))
      
      // bracketted expressions; arithmetic to bool conversion
      val queryString4 = """
        SELECT ?A ?B ?C
      	WHERE {
          ?A <http://nr1> ?B
          FILTER(4*(7-3) && (12-(4*4)))
          FILTER(((((1)))))
          ?A <http://nr2> ?C
          FILTER((?B*2-?C)*(?B-?C))
        }"""
      
      val query4 = Sparql(queryString4).get
      val result4 = query4.resultIterator.toList
      
      assert(result4.length == 3)
      result4.foreach( e => assert(e("B") != "9" && e("B") != "36") )
      
      // global filter w/ brackets
      val queryString5 = """
        SELECT ?A ?B ?C
      	WHERE {
          ?A <http://nr1> ?B .
          FILTER(?B = ?B && (5 = 0 || (?C != 8974 && -3 < 3/5)))
          FILTER((5+3)*4 = 32 && (?B != -2321 && (1+1)*(48/4) = 2*(3*4)))
          ?A <http://nr2> ?C
        }"""
      
      val query5 = Sparql(queryString5).get
      val result5 = query5.resultIterator.toList
      
      assert(result5.length == 5)
      
    } finally {
      tr.shutdown
    }
  }

}
