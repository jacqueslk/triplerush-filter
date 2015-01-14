package com.signalcollect.triplerush

sealed trait PrimaryExpression {
  def getValue(bindings: Map[Int, String]): Any
}
case class BuiltInCall(name: String) extends PrimaryExpression {
  def getValue(bindings: Map[Int, String]): Any = {
    throw new Exception("Built in calls are not yet supported")
  }
}
case class Var(index: Int) extends PrimaryExpression {
  def getValue(bindings: Map[Int, String]): Any = {
    try {
      // Only supports numbers (Double) right now...
      val number = bindings.get(index).get.toDouble
      return number
    } catch {
      case e: Exception => 
        println("getValue: Encountered " + e.getClass.getSimpleName + s" for $this with bindings $bindings")
      return None
    }
  }
}
case class NumericLiteral(number: Double) extends PrimaryExpression {
  def getValue(bindings: Map[Int, String]): Double = number
}

// String1 = * or /, empty for first entry
// String2 = !, +, - or empty, it is a prefix to the primary expression
case class MultiplicativeExpression(entries: Seq[(String, String, PrimaryExpression)]) {
  def getVariableSet: Set[Int] = {
    Set() ++ entries.collect {
      case (s: String, t: String, v: Var) => v.index
    }
  }
  def getValue(bindings: Map[Int, String]): Any = {
    val firstResult = applyPrimaryFlag(entries(0)._2, entries(0)._3.getValue(bindings))
    if (entries.size == 1) return firstResult
    else if (!firstResult.isInstanceOf[Double]) return None // EBV.Error
    
    var result = 0.0
    firstResult match {
      case double: Double => result = double
      case _ => return None
    }
        
    for (i <- 1 until entries.length) {
      val entryValue = applyPrimaryFlag(entries(i)._2, entries(i)._3.getValue(bindings))
      if (!entryValue.isInstanceOf[Double]) return None // EBV.Error
      
      if (entries(i)._1 == "*") result *= entryValue.asInstanceOf[Double]
      else                      result /= entryValue.asInstanceOf[Double]
    }
    result
  }
  private def applyPrimaryFlag(prefix: String, expressionValue: Any): Any = {
    if (prefix == "") {
      expressionValue
    } else if (prefix == "!") {
      expressionValue match {
        case bool: Boolean => return !bool
        case _ => println(s"MultiplicativeExpression: ! operator not applicable to primary expression $expressionValue")
      }
      return None
    } else if (prefix == "+" || prefix == "-") {
      val multiplier = if (prefix == "-") -1 else 1
      expressionValue match {
        case double: Double => return multiplier * double
        case int: Int => return multiplier * int
        case _ => println(s"MultiplicativeExpression: + or - cannot be used for primary expression $expressionValue")
      }
      return None
    } else {
      throw new Exception(s"Unknown prefix $prefix for $expressionValue")
    }
  }
}
case class AdditiveExpression(entries: Seq[(String, MultiplicativeExpression)]) {
  def getVariableSet: Set[Int] = {
    Set() ++ entries.flatMap {
      entry => entry._2.getVariableSet 
    }
  }
  def getValue(bindings: Map[Int, String]): Any = {
    if (entries.length == 1) { // if only one entry, forward the value as it is
      entries(0)._2.getValue(bindings)
    }
    else { // if we have multiple entries, we MUST have a numberical result
      computeNumericResult(bindings)
    }
  }
  private def computeNumericResult(bindings: Map[Int, String]): Any = {
    var result = 0.0
    entries.foreach {
      multExpr =>
        val multExprVal = multExpr._2.getValue(bindings)
        if (!multExprVal.isInstanceOf[Double]) return None
        if (multExpr._1 == "-") {
          result -= multExprVal.asInstanceOf[Double]
        } else { // matches "+" as well as first empty entry
          result += multExprVal.asInstanceOf[Double]
        }
    }
    result
  }
}
case class RelationalExpression(lhs: AdditiveExpression, operator: String, rhs: Option[AdditiveExpression]) {
  def getVariableSet: Set[Int] = {
    val lhsSet = lhs.getVariableSet
    val rhsSet = if(rhs.isDefined) rhs.get.getVariableSet else Set[Int]()
    (lhsSet ++ rhsSet)
  }
  def getValue(bindings: Map[Int, String]): Any = {
    if (rhs.isDefined) {
      checkArithmeticOperator(lhs.getValue(bindings), rhs.get.getValue(bindings))
    }
    else {
      lhs.getValue(bindings)
    }
  }
  // http://www.w3.org/TR/rdf-sparql-query/#OperatorMapping
  private def checkArithmeticOperator(lhsValue: Any, rhsValue: Any): Boolean = {
    if (lhsValue.getClass != rhsValue.getClass) {
      println(s"Error for constraint $lhsValue $operator $rhsValue: Not same type")
      return false // EBV.Error
    }
    operator match {
      case "="  => (lhsValue == rhsValue)
      case "!=" => (lhsValue != rhsValue)
      case "<"  => lessThan(lhsValue, rhsValue)
      case ">"  => greaterThan(lhsValue, rhsValue)
      case "<=" => (lhsValue == rhsValue) || lessThan(lhsValue, rhsValue)
      case ">=" => (lhsValue == rhsValue) || greaterThan(lhsValue, rhsValue)
      case _ => throw new Exception(s"Unknown operator $operator")
    }
  }
  private def lessThan(lhs: Any, rhs: Any): Boolean = {
    (lhs, rhs) match {
      case (lhsB: Boolean, rhsB: Boolean) => (!lhsB && rhsB)
      case (lhsD: Double, rhsD: Double)   => (lhsD < rhsD)
      case (lhsS: String, rhsS: String)   => (lhsS.compareTo(rhsS) < 0)
      case _ => throw new Exception(s"Unknown type combination for lhs=$lhs and rhs=$rhs!")
    }
  }
  private def greaterThan(lhs: Any, rhs: Any): Boolean = {
    (lhs, rhs) match {
      case (lhsB: Boolean, rhsB: Boolean) => (lhsB && !rhsB)
      case (lhsD: Double, rhsD: Double)   => (lhsD > rhsD)
      case (lhsS: String, rhsS: String)   => (lhsS.compareTo(rhsS) > 0)
      case _ => throw new Exception(s"Unknown type combination for lhs=$lhs and rhs=$rhs!")
    }
  }
}
case class ConditionalAndExpression(entries: Seq[RelationalExpression]) {
  def getValue(bindings: Map[Int, String]): Any = {
    if (entries.size == 1) {
     entries(0).getValue(bindings) 
    } else {
      entries.foreach {
        relExpr => if (!Filter.effectiveBooleanValue(relExpr.getValue(bindings))) return false
      }
      true
    }
  }
}

sealed trait Constraint {
  def getValue(bindings: Map[Int, String]): Any
}
case class ConditionalOrExpression(entries: Seq[ConditionalAndExpression]) extends Constraint with PrimaryExpression {
  def getValue(bindings: Map[Int, String]): Any = {
    if (entries.size == 1) {
      entries(0).getValue(bindings)
    } else {
      entries.foreach {
        condAnd => if (Filter.effectiveBooleanValue(condAnd.getValue(bindings))) {
          return true
        }
      }
      false
    }
  }
}
case class GlobalNegative() extends Constraint {
    def passes(bindings: Map[Int, String]): Boolean = false
    def getValue(bindings: Map[Int, String]): Boolean = false
}


object Filter {
  def globalFalse(): Filter = {
    Filter(GlobalNegative())
  }
  /**
   * Converts a variable to the boolean value as per
   * http://www.w3.org/TR/rdf-sparql-query/#ebv
   */
  def effectiveBooleanValue(value: Any): Boolean = {
    value match {
      case bool: Boolean  => bool
      case string: String => (string.length > 0)
      case double: Double => (double != 0.0)
      case int: Integer   => (int != 0)
      case _ => throw new Exception(s"Unexpected value $value of type " + value.getClass.getSimpleName)
    }
  }
}


case class Filter(constraint: Constraint) {
  import Filter.globalFalse
  
  def isGlobalFalse: Boolean = constraint.isInstanceOf[GlobalNegative]
  def isArithmetic:  Boolean = constraint.isInstanceOf[ConditionalOrExpression]
  
  /**
   * Returns a set of all variables the filter requires
   * to be evaluated
   */
  def getVariableSet: Set[Int] = {
    if (isArithmetic) {
      val condOr = constraint.asInstanceOf[ConditionalOrExpression]
      val result = Set() ++ condOr.entries.flatMap {
        condAnd => condAnd.entries.flatMap {
          relExpr => relExpr.getVariableSet
        }
      }
      return result
    }
    Set()
  }
  
  /**
   * Evaluates a filter with the bindings given in `bindings`
   * and returns whether or not the filter passes.
   * `bindings` should only contain known bindings, i.e. one
   * should check with `getVariableSet()` whether or not the
   * filter can be evaluated with the current bindings.
   * @param bindings: Map with variable name and value,
   *  e.g. B => 12 if ?B is 12.
   */
  def passes(bindings: Map[Int, String]): Boolean = {
    Filter.effectiveBooleanValue(constraint.getValue(bindings))
  }
}