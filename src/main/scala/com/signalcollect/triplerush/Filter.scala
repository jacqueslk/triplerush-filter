package com.signalcollect.triplerush

/**
 * Trait to group different tree elements with one
 * type together. Implementation of the "PrimaryExpression"
 * grammar rule of the SPARQL grammar.
 */
sealed trait PrimaryExpression {
  // In the future, it might be interesting to add a
  // getVariableSet() method for all primary expressions,
  // so `MultiplicativeExpression` doesn't have to be
  // expanded to know all of its children's underlying types.
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
      // Assumes that all bindings are doubles
      // Problem: dictionary saves all values as string, so
      // currently no differentiation between "3" and 3 possible
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

/**
 * Represents one or more primary expressions connected with multiplication
 * or division.
 * The string in the first field is * or /; it is empty for first entry
 * The string in the second field represents a possible unary operator
 * before the primary expression in the third field. The operator may be
 * !, +, - or empty. (Cf. "UnaryExpression" in SPARQL spec)
 * For instance, `entries` for "5 * -?A" is something like:
 *    ("",   "", NumericLiteral(5)),
 *    ("*", "-", Var(-2))
 */
case class MultiplicativeExpression(entries: Seq[(String, String, PrimaryExpression)]) {
  def getVariableSet: Set[Int] = {
    Set() ++ entries.flatMap {
      case (s: String, t: String, v: Var) => List(v.index)
      case (s: String, t: String, e: ConditionalOrExpression) => e.getVariableSet
      case _ => Nil
    }
  }
  def getValue(bindings: Map[Int, String]): Any = {
    val firstResult = applyUnaryFlag(entries(0)._2, entries(0)._3.getValue(bindings))
    if (entries.size == 1) return firstResult
    else if (!firstResult.isInstanceOf[Double]) return None // error
    
    var result = 0.0
    firstResult match {
      case double: Double => result = double
      case _ => return None
    }
        
    for (i <- 1 until entries.length) {
      val entryValue = applyUnaryFlag(entries(i)._2, entries(i)._3.getValue(bindings))
      if (!entryValue.isInstanceOf[Double]) return None // error
      
      if (entries(i)._1 == "*") result *= entryValue.asInstanceOf[Double]
      else                      result /= entryValue.asInstanceOf[Double]
    }
    result
  }
  private def applyUnaryFlag(prefix: String, expressionValue: Any): Any = {
    if (prefix == "") {
      expressionValue
    }
    else if (prefix == "!") {
      if (expressionValue == None) return None
      else  return !(Filter.effectiveBooleanValue(expressionValue))
    }
    else if (prefix == "+" || prefix == "-") {
      val multiplier = if (prefix == "-") -1 else 1
      expressionValue match {
        case double: Double => return multiplier * double
        case int: Int => return multiplier * int
        case _ => println(s"MultiplicativeExpression: + or - cannot be used for primary expression $expressionValue")
      }
      return None
    }
    else {
      throw new Exception(s"Unknown prefix $prefix for $expressionValue")
    }
  }
}
/**
 * Represents addition and subtraction among multiplicative expressions,
 * e.g. 5*3 + ?A - ?B/3, where entries would be something like
 * {("", 5*3), ("+", ?A), ("-", ?B/3)}.
 * String in first field of `entries` is either + or -;
 * it is empty for the first entry.
 */
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
    else { // if we have multiple entries, we MUST have a numeric result
      computeNumericResult(bindings)
    }
  }
  private def computeNumericResult(bindings: Map[Int, String]): Any = {
    var result = 0.0
    entries.foreach {
      multExpr =>
        val multExprVal = multExpr._2.getValue(bindings)
        if (!multExprVal.isInstanceOf[Double]) return None // error
        if (multExpr._1 == "-") {
          result -= multExprVal.asInstanceOf[Double]
        } else { // matches "+" as well as first empty entry
          result += multExprVal.asInstanceOf[Double]
        }
    }
    result
  }
}
/**
 * Handles comparisons with the operators =, !=, <, >, <= and >=.
 * `lhs` is the child on the left-hand side;
 * `operator` is a String with the operator, or empty if `rhs` is unset
 * `rhs` is the child on the right-hand side.
 * If `rhs` is empty, `lhs` represents the only child of the node and
 * evaluates to the value of `lhs`. If `rhs` is set, the node evaluates
 * to a Boolean.
 */
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
    if (lhsValue.getClass != rhsValue.getClass || lhsValue == None) {
      println(s"Error for constraint $lhsValue $operator $rhsValue: Not same type or None")
      return false // error
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
/**
 * Node representing one or more relational expressions combined with
 * the Boolean operator &&, i.e. if it has multiple children, it evaluates
 * to true if all children are true and false otherwise. If it has one
 * child, it simply forwards the child's value.
 * e.g. "?A+4>0 && ?A!=?B" would result in the following two children in 
 * `entries`: (?A+4 > 0) and (?A != ?B)
 */
case class ConditionalAndExpression(entries: Seq[RelationalExpression]) {
  def getValue(bindings: Map[Int, String]): Any = {
    if (entries.size == 1) {
     entries(0).getValue(bindings) // Forward value if only child
    } else {
      entries.foreach {
        relExpr => if (!Filter.effectiveBooleanValue(relExpr.getValue(bindings))) return false
      }
      true
    }
  }
  def getVariableSet: Set[Int] = {
    Set() ++ entries.flatMap {
      relExpr => relExpr.getVariableSet
    }
  }
}
/**
 * All elements should implement this trait if they should be able to
 * be a Filter object's child. See rule [27] Constraint in SPARQL spec.
 * BracketedExpression is skipped and we use ConditionalOrExpression
 * directly instead.
 */
sealed trait Constraint {
  def getValue(bindings: Map[Int, String]): Any
  def getVariableSet: Set[Int]
}

/**
 * Represents one or more conditional-and expressions which are 
 * chained together with the Boolean || operator.
 * This is also the element used for BracketedExpression and therefore
 * uses the traits `Constraint` and `PrimaryExpression` as well.
 */
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
  def getVariableSet: Set[Int] = {
    Set() ++ entries.flatMap {
      condAnd => condAnd.getVariableSet
    }
  }
}

/**
 * Dummy filter placed at the front of a query's filter list by
 * the dictionary vertex to halt query execution if there is a
 * filter which always evaluates to false.
 */
case class GlobalNegative() extends Constraint {
    def getValue(bindings: Map[Int, String]): Boolean = false
    def getVariableSet: Set[Int] = Set()
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
      case None => false // Difference to spec: `error' gets converted to false
      case _ => throw new Exception(s"Unexpected value $value of type " + value.getClass.getSimpleName)
    }
  }
}

/**
 * Main class representing a SPARQL filter. This is the only
 * class which is relevant for communication with the 
 * "outside world."
 */
case class Filter(constraint: Constraint) {
  import Filter.globalFalse
  
  /**
   * Returns true if the filter is of type GlobalNegative;
   * false otherwise. This is used by the dictionary vertex.
   */
  def isGlobalFalse: Boolean = constraint.isInstanceOf[GlobalNegative]
  
  /**
   * Returns a set of all variables the filter requires
   * to be evaluated
   */
  def getVariableSet: Set[Int] = {
    constraint.getVariableSet
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