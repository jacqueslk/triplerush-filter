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
                    e("B").toInt + e("C").toInt > 50
                 && e("B").toInt < 40
                 && e("C").toInt >= 20
      ))
      
    } finally {
      tr.shutdown
    }
  }
  
  it should "process the < filter" in {
    implicit val tr = prepareTripleRush
    try {
      val queryString = """
        SELECT ?A ?T ?B
      	WHERE {
          <http://a> <http://p> ?A .
          ?A ?T ?B .
          FILTER(?B < 14)
        }"""
      val query = Sparql(queryString).get
      val result = query.resultIterator.toList
      assert(result.length == 1)
      result.foreach( e => assert(e("B").toInt < 14) )
    } finally {
      tr.shutdown
    }
  }
  
  it should "process the != filter" in {
    implicit val tr = prepareTripleRush
    try {
      val queryString = """
        SELECT ?A ?T ?B
      	WHERE {
          <http://a> <http://p> ?A .
          FILTER(?B != 1)
          ?A ?T ?B .
        }"""
      val query = Sparql(queryString).get
      val result = query.resultIterator.toList
      assert(result.length == 2)
      result.foreach( e => assert(e("B").toInt != 1) )
    } finally {
      tr.shutdown
    }
  }
  
  it should "process the = filter" in {
    implicit val tr = prepareTripleRush
    try {
      val queryString = """
        SELECT ?A ?T ?B
      	WHERE {
          <http://a> <http://p> ?A .
          FILTER(?B = 123)
          ?A ?T ?B
        }"""
      val query = Sparql(queryString).get
      val result = query.resultIterator.toList
      assert(result.length == 1)
      result.foreach( e => assert(e("B").toInt == 123) )
    } finally {
      tr.shutdown
    }
  }
  
  it should "process the > filter" in {
    implicit val tr = prepareTripleRush
    try {
      val queryString = """
        SELECT ?A ?T ?B
      	WHERE {
          <http://a> <http://p> ?A
          FILTER(?B > 14)
          ?A ?T ?B
        }"""
      val query = Sparql(queryString).get
      val result = query.resultIterator.toList
      assert(result.length == 1)
      result.foreach( e => assert(e("B").toInt > 14) )
    } finally {
      tr.shutdown
    }
  }
  
  it should "process the >= filter" in {
    implicit val tr = prepareTripleRush
    try {
      val queryString = """
        SELECT ?A ?T ?B
      	WHERE {
          FILTER(?B >= 125)
          <http://a> <http://p> ?A .
          ?A ?T ?B
        }"""
      val query = Sparql(queryString).get
      val result = query.resultIterator.toList
      assert(result.length == 0)
      result.foreach( e => assert(e("B").toInt >= 125) )
    } finally {
      tr.shutdown
    }
  }
  
  it should "process filters with no variables" in {
    implicit val tr = prepareTripleRush
    try {
      val impossibleQuery = """
        SELECT ?A ?T ?B
      	WHERE {
          FILTER(1 > 5)
          <http://a> <http://p> ?A .
          ?A ?T ?B
        }"""
      val query1 = Sparql(impossibleQuery).get
      val result1 = query1.resultIterator.toList
      assert(result1.length == 0)
      
      val possibleQuery = """
        SELECT ?A ?T ?B
      	WHERE {
          FILTER(120 >= 120)
          <http://a> <http://p> ?A .
          ?A ?T ?B .
        }"""
      val query2 = Sparql(possibleQuery).get
      val result2 = query2.resultIterator.toList
      assert(result2.length == 3)
      
      val possibleCombinedQuery = """
        SELECT ?A ?T ?B
      	WHERE {
          <http://a> <http://p> ?A
          FILTER(130 != 140)
          ?A ?T ?B
          FILTER(?B <= 14)
        }"""
      val query3 = Sparql(possibleCombinedQuery).get
      val result3 = query3.resultIterator.toList
      assert(result3.length == 2)
      result3.foreach( e => assert(e("B").toInt <= 14) )
    } finally {
      tr.shutdown
    }
  }

}
