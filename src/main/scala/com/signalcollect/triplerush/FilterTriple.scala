package com.signalcollect.triplerush

object FilterTriple {
  
  val op2int = Map(
    "="  -> 1,
    ">"  -> 2,
    "<"  -> 3,
    "!=" -> 4,
    ">=" -> 5,
    "<=" -> 6   
  )
  val int2op = op2int.map(_.swap)
  
  def operatorToInt(op: String): Int = {
    val opNumber = op2int.get(op)
    if (opNumber.isDefined) return opNumber.get
    else throw new Exception(s"Unsupported operator $op")
  }
  
  def operatorToInt(op: String, leftIsVar: Boolean, rightIsVar: Boolean): Int = {
    val opNumber = operatorToInt(op)
    val lhsFlag = if(leftIsVar)  0x80000000 else 0
    val rhsFlag = if(rightIsVar) 0x40000000 else 0
    (opNumber | rhsFlag | lhsFlag)
  }
  
  def intToOperator(number: Int): String = {
    val comparator = int2op.get((number & 0x1fffffff))
    if (comparator.isDefined) return comparator.get
    else throw new Exception(s"Unknown comparator number $number")
  }
}


/*
 * Bit flags:
 * 0x8000_0000: 1 =  left-hand side is variable; 0 = lhs is value
 * 0x4000_0000: 1 = right-hand side is variable; 0 = rhs is value
 * 0x2000_0000: 1 if function is negated, 0 if not. (tentative, not implemented yet)
 */
case class FilterTriple(lhs: Int, comparator: Int, rhs: Int) {
   import FilterTriple._
  
   def intToOperator: String = FilterTriple.intToOperator(comparator)
   
   def lhsIsVar:  Boolean = (comparator & 0x80000000) == 0x80000000
   def rhsIsVar:  Boolean = (comparator & 0x40000000) == 0x40000000
   def isNegated: Boolean = (comparator & 0x20000000) == 0x20000000
   
   def comparatorNoFlags: Int = (comparator & 0x1ffffff)
   
   def isArithmeticFilter: Boolean = (1 <= comparator && comparator <= 6)
   
   def passes(lhsVal: Option[Int] = None, rhsVal: Option[Int] = None): Boolean = {
     // Ensure that we have the values we need for the variables
     if (lhsIsVar && !lhsVal.isDefined || rhsIsVar && !rhsVal.isDefined) {
       throw new Exception("Variables without values encountered!")
     }
     if (isArithmeticFilter) {
       return arithmeticFilter(lhsVal, rhsVal)
     }
     else {
       throw new Exception("Unknown filter type!")
     }
   }
   
   def arithmeticFilter(lhsVal: Option[Int] = None, rhsVal: Option[Int] = None): Boolean = {
     val lhs = if(lhsVal.isDefined) lhsVal.get else this.lhs
     val rhs = if(rhsVal.isDefined) rhsVal.get else this.rhs
    
    comparator match {
      case 1 =>  lhs == rhs
      case 2 =>  lhs >  rhs
      case 3 =>  lhs <  rhs
      case 4 =>  lhs != rhs
      case 5 =>  lhs >= rhs
      case 6 =>  lhs <= rhs
      case _ => throw new Exception(s"Unknown operator $comparator")
    }
  }
}