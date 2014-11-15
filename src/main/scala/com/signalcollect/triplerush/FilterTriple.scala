package com.signalcollect.triplerush

object FilterTriple {
  def operatorToInt(op: String) : Int = {
    op match {
      case "="  => 1
      case ">"  => 2
      case "<"  => 3
      case "!=" => 4
      case ">=" => 5
      case "<=" => 6
      case _ => throw new Exception("Unsupported operator")
    }
  }
   
  def apply(entry: String, comparator: String, value: String): FilterTriple = {
    val entryIndex = entry.toInt         // for now...
    val comparatorIndex = operatorToInt(comparator)
    val valueIndex = value.toInt         // for now...
    FilterTriple(entryIndex, comparatorIndex, valueIndex)
  }
}

case class FilterTriple(entry: Int, comparator: Int, value: Int) {

}