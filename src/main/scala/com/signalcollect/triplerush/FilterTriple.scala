package com.signalcollect.triplerush

sealed trait UnaryExpression {
  def getRealValue(bindings: Map[Int, String]): Option[Double]
}
case class BuiltInCall(name: String) extends UnaryExpression {
  def getRealValue(bindings: Map[Int, String]): Option[Double] = {
    throw new Exception("Built in call not yet supported")
  }
}
case class Var(index: Int) extends UnaryExpression {
  def getRealValue(bindings: Map[Int, String]): Option[Double] = {
    try {
      val number = bindings.get(index).get.toDouble
      return Some(number)
    } catch {
      case e: Exception => 
        println("getRealValue: Encountered " + e.getClass.getSimpleName() + s" for $this with bindings $bindings")
      return None
    }
  }
}
case class NumericLiteral(number: Int) extends UnaryExpression {
  def getRealValue(bindings: Map[Int, String]): Option[Double] = Some(number)
}

case class MultiplicativeExpression(entries: Seq[(String, UnaryExpression)]) {
  def getVariableSet: Set[Int] = {
    Set() ++ entries.collect {
      case (s: String, v: Var) => v.index
    }
  }
  def compute(bindings: Map[Int, String]): Option[Double] = {
    val optionResult = entries(0)._2.getRealValue(bindings)
    if (!optionResult.isDefined) return None
    var result = optionResult.get
    
    for (i <- 1 until entries.length) {      
      val entry = entries(i)
      val realValue = entry._2.getRealValue(bindings)
      if (!realValue.isDefined) return None
      
      if (entry._1 == "*") result *= realValue.get
      else                 result /= realValue.get
    }
    Some(result)
  }
}
case class AdditiveExpression(entries: Seq[(String, MultiplicativeExpression)]) {
  def getVariableSet: Set[Int] = {
    Set() ++ entries.flatMap {
      entry => entry._2.getVariableSet 
    }
  }
  def compute(bindings: Map[Int, String]): Option[Double] = {
    var result = 0.0
    entries.foreach {
      multExpr =>
        val multExprVal = multExpr._2.compute(bindings)
        if (!multExprVal.isDefined) return None
        if (multExpr._1 == "-") {
          result -= multExprVal.get
        } else { // matches "+" as well as first empty entry
          result += multExprVal.get
        }
    }
    Some(result)
  }
}
case class RelationalExpression(lhs: AdditiveExpression, operator: String, rhs: Option[AdditiveExpression]) {
  def getVariableSet: Set[Int] = {
    val lhsSet = lhs.getVariableSet
    val rhsSet = if(rhs.isDefined) rhs.get.getVariableSet else Set[Int]()
    (lhsSet ++ rhsSet)
  }
  def passes(bindings: Map[Int, String]): Boolean = {
    if (!rhs.isDefined) return false // ?
    val lhsVal = lhs.compute(bindings)
    val rhsVal = rhs.get.compute(bindings)
    if (!lhsVal.isDefined || !rhsVal.isDefined) return false
    checkArithmeticOperator(lhsVal.get, rhsVal.get)
  }
  private def checkArithmeticOperator(lhsValue: Double, rhsValue: Double): Boolean = {
    operator match {
      case "="  => lhsValue == rhsValue
      case ">"  => lhsValue >  rhsValue
      case "<"  => lhsValue <  rhsValue
      case "!=" => lhsValue != rhsValue
      case ">=" => lhsValue >= rhsValue
      case "<=" => lhsValue <= rhsValue
      case _ => throw new Exception(s"Unknown operator $operator")
    }
  }
}
case class ConditionalAndExpression(entries: Seq[RelationalExpression]) {
  def passes(bindings: Map[Int, String]): Boolean = {
    entries.foreach {
      relExpr => if (!relExpr.passes(bindings)) return false
    }
    true
  }
}

sealed trait Constraint {
  def passes(bindings: Map[Int, String]): Boolean
}
case class ConditionalOrExpression(entries: Seq[ConditionalAndExpression]) extends Constraint {
  def passes(bindings: Map[Int, String]): Boolean = {
    entries.foreach {
      condAnd => if (condAnd.passes(bindings)) {
        return true
      }
    }
    false
  }
}
case class GlobalNegative() extends Constraint {
    def passes(bindings: Map[Int, String]): Boolean = false
}


object FilterTriple {
  def globalFalse(): FilterTriple = {
    FilterTriple(GlobalNegative())
  }
}


case class FilterTriple(constraint: Constraint) {
  import FilterTriple.globalFalse
  
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
    constraint.passes(bindings)
  }
}