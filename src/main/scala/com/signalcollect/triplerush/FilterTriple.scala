package com.signalcollect.triplerush

object FilterTriple {
  
  val op2int = Map(
    "=" -> 1,
    ">" -> 2,
    "<" -> 3,
    "!=" -> 4,
    ">=" -> 5,
    "<=" -> 6   
  )
  val int2op = op2int.map(_.swap)
  
  def operatorToInt(op: String) : Int = {
    val opNumber = op2int.get(op)
    if (opNumber.isDefined) return opNumber.get
    else throw new Exception(s"Unsupported operator $op")
  }
  
  def intToOperator(number: Int) : String = {
    val comparator = int2op.get(number)
    if (comparator.isDefined) return comparator.get
    else throw new Exception(s"Unknown comparator number $number")
  }
   
  def apply(entry: String, comparator: String, value: String): FilterTriple = {
    val entryIndex = entry.toInt         // for now...
    val comparatorIndex = operatorToInt(comparator)
    val valueIndex = value.toInt         // for now...
    FilterTriple(entryIndex, comparatorIndex, valueIndex)
  }
}

case class FilterTriple(entry: Int, comparator: Int, value: Int) {
   import FilterTriple._
  
   def intToOperator : String = FilterTriple.intToOperator(comparator)

}