package com.signalcollect.triplerush

object FilterTriple {
  def operatorToInt(op: String) : Int = {
    op match {
      case "=" => 0
      case ">" => 1
      case "<" => 2
      case _ => throw new Exception("Unsupported operator")
    }
  }
   
  def apply(item: String, op: String, value: String): FilterTriple = {
    val obj = item.toInt // for now...
    val operator = operatorToInt(op)
    val comp = value.toInt // for now...
    new FilterTriple(obj, operator, comp)
  }
}

case class FilterTriple(item: Int, operator: Int, value: Int) {

}