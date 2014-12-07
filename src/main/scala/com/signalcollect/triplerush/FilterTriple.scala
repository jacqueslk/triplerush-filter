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
  import FilterTriple._
  
  def isGlobalFalse: Boolean = constraint.isInstanceOf[GlobalNegative]
  def isArithmetic:  Boolean = constraint.isInstanceOf[ConditionalOrExpression]
  
  /**
   * Returns a list of all variables 
   */
//  def getVariableSet: Set[String] = {
//    val varSet = scala.collection.mutable.Set[String]()
//    if (isArithmetic) {
//      val condOr = constraint.asInstanceOf[ConditionalOrExpression]
//      condOr.entries.foreach {
//        condAnd => condAnd.entries.foreach {
//            relExp => varSet ++ getVariableSetForRelExpr(relExp)
//        }
//      }
//    }
//    Set()
//  }
  
  /**
   * Gets the variable set (i.e. a set of all variables required to evaluate the expression)
   * for a relational expression
   */
//  private def getVariableSetForRelExpr(relExpr: RelationalExpression): Set[String] = {
//    
//  }
  
//  private def getVariableSetForMultExpr(multExpr: MultiplicativeExpression): Set[String] = {
//    val varSet = scala.collection.mutable.Set[String]()
//    if (multExpr.firstValue.isInstanceOf[Var]) varSet += multExpr.firstValue.asInstanceOf[Var].name
//    multExpr.otherValues.foreach {
//      primExpr => if (primExpr.isInstanceOf[Var]) varSet += multExpr.firstValue.asInstanceOf[Var].name
//    }
//    varSet.toSet
//  }

}