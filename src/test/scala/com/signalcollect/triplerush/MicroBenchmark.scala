package com.signalcollect.triplerush

import com.signalcollect.triplerush.sparql.Sparql

object MicroBenchmark extends App {

  implicit val tr = new TripleRush
  Lubm.load(tr)
  val runs = 100
  val bestTime = (1 to runs).map(x => runAllLubmQueries).min
  println(s"Best time: $bestTime milliseconds")
  tr.shutdown

  def runAllLubmQueries: Double = {
    val startTime = System.nanoTime
    for (queryString <- Lubm.sparqlQueries) {
      val query = Sparql(queryString).get
      val results = query.encodedResults
      var resultCounter = 0
      while (results.hasNext) {
        resultCounter += 1
        results.next
      }
    }
    val finishTime = System.nanoTime
    val totalTimeInMs = ((finishTime - startTime) / 1e5).round / 10.0
    totalTimeInMs
  }

}