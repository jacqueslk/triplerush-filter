package com.signalcollect.triplerush

sealed trait UnaryExpression
case class BuiltInCall(name: String) extends UnaryExpression
case class Var(name: String) extends UnaryExpression
case class NumericLiteral(number: Int) extends UnaryExpression

case class MultiplicativeExpression(entries: Seq[(String, UnaryExpression)])
case class AdditiveExpression(entries: Seq[(String, MultiplicativeExpression)])
case class RelationalExpression(lhs: AdditiveExpression, operator: String, rhs: Option[AdditiveExpression])
case class ConditionalAndExpression(entries: Seq[RelationalExpression])

sealed trait Constraint
case class ConditionalOrExpression(entries: Seq[ConditionalAndExpression]) extends Constraint
case class GlobalNegative() extends Constraint


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
  def getVariableSet: Set[String] = {
    if (isArithmetic) {
      val condOr = constraint.asInstanceOf[ConditionalOrExpression]
      val result = Set() ++ condOr.entries.flatMap {
        condAnd => condAnd.entries.flatMap {
          relExpr => getVariableSetForRelExpr(relExpr)
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
  def passes(bindings: Map[String, String]): Boolean = {
    if (isArithmetic) {
      constraint.asInstanceOf[ConditionalOrExpression].entries.foreach {
        condAnd => if (passesForCondAnd(condAnd, bindings)) {
          return true
        }
      }
      false
    } else if (isGlobalFalse) {
      false
    } else {
      true
    }
  }
  
  
  // ===================
  // Passes
  // ===================
  private def passesForCondAnd(condAnd: ConditionalAndExpression, bindings: Map[String, String]): Boolean = {
    condAnd.entries.foreach {
      relExpr => if (!passesForRelExpr(relExpr, bindings)) return false
    }
    true
  }
  
  private def passesForRelExpr(relExpr: RelationalExpression, bindings: Map[String, String]): Boolean = {
    if (!relExpr.rhs.isDefined) return false // ?
    val lhs = computeAddExpr(relExpr.lhs, bindings)
    val rhs = computeAddExpr(relExpr.rhs.get, bindings)
    if (!lhs.isDefined || !rhs.isDefined) return false
    checkArithmeticOperator(lhs.get, relExpr.operator, rhs.get)
  }
  
  private def checkArithmeticOperator(lhs: Double, operator: String, rhs: Double): Boolean = {
    operator match {
      case "="  => lhs == rhs
      case ">"  => lhs >  rhs
      case "<"  => lhs <  rhs
      case "!=" => lhs != rhs
      case ">=" => lhs >= rhs
      case "<=" => lhs <= rhs
      case _ => throw new Exception(s"Unknown operator $operator")
    }
  }
  
  private def computeAddExpr(addExpr: AdditiveExpression, bindings: Map[String, String]): Option[Double] = {
    var result = 0.0
    addExpr.entries.foreach {
      multExpr =>
        val multExprVal = computeMultExpr(multExpr._2, bindings)
        if (!multExprVal.isDefined) return None
        if (multExpr._1 == "-") {
          result -= multExprVal.get
        } else { // matches "+" as well as first empty entry
          result += multExprVal.get
        }
    }
    Some(result)
  }
  
  private def computeMultExpr(multExpr: MultiplicativeExpression, bindings: Map[String, String]): Option[Double] = {
    val optionResult = getRealValue(multExpr.entries(0)._2, bindings)
    if (!optionResult.isDefined) return None
    var result = optionResult.get
    
    for (i <- 1 until multExpr.entries.length) {      
      val entry = multExpr.entries(i)
      val realValue = getRealValue(entry._2, bindings)
      if (!realValue.isDefined) return None
      
      if (entry._1 == "*") result *= realValue.get
      else                 result /= realValue.get
    }
    Some(result)
  }
  
  private def getRealValue(unary: UnaryExpression, bindings: Map[String, String]): Option[Double] = {
    if (unary.isInstanceOf[Var]) {
      try {
        val number = bindings.get(unary.asInstanceOf[Var].name).get.toDouble
        return Some(number)
      } catch { case _: Exception => return None }
    }
    else if (unary.isInstanceOf[NumericLiteral]) {
      return Some(unary.asInstanceOf[NumericLiteral].number)
    }
    None
  }
  
  // ===================
  // Variable Set
  // ===================
  /**
   * Gets the variable set of a RelationalExpression object
   */
  private def getVariableSetForRelExpr(relExpr: RelationalExpression): Set[String] = {
    val lhsSet = getVariableSetForAddExpr(relExpr.lhs)
    val rhsSet = if(relExpr.rhs.isDefined) getVariableSetForAddExpr(relExpr.rhs.get) else Set[String]()
    (lhsSet ++ rhsSet)
  }
  
  /**
   * Gets the variable set of an AdditiveExpression object
   */
  private def getVariableSetForAddExpr(addExpr: AdditiveExpression): Set[String] = {
    Set() ++ addExpr.entries.flatMap {
      entry => getVariableSetForMultExpr(entry._2) 
    }
  }
  
  /**
   * Gets the variable set of a MultiplicativeExpression object
   */
  private def getVariableSetForMultExpr(multExpr: MultiplicativeExpression): Set[String] = {
    Set() ++ multExpr.entries.collect {
      case (s: String, v: Var) => v.name
    }
  }

}