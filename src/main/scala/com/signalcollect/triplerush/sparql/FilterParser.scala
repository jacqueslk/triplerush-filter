package com.signalcollect.triplerush.sparql

import scala.util.parsing.combinator.RegexParsers

// Filter Triple case classes / traits
import com.signalcollect.triplerush.PrimaryExpression
import com.signalcollect.triplerush.BuiltInCall
import com.signalcollect.triplerush.Var
import com.signalcollect.triplerush.NumericLiteral

import com.signalcollect.triplerush.MultiplicativeExpression
import com.signalcollect.triplerush.AdditiveExpression
import com.signalcollect.triplerush.RelationalExpression
import com.signalcollect.triplerush.ConditionalAndExpression

import com.signalcollect.triplerush.Constraint
import com.signalcollect.triplerush.ConditionalOrExpression
//import com.signalcollect.triplerush.GlobalNegative

case class FilterParser(variableNameToId: Map[String, Int]) extends RegexParsers {
  val identifier: Parser[String] = "[a-zA-Z0-9]*".r
  //val integer: Parser[String] = "-?[0-9]+".r
  //val decimal: Parser[String] = "-?[0-9]*\\.[0-9]+"
  
  def integer: Parser[Int] = "\\-?[0-9]+".r ^^ (_.toInt)

  def double: Parser[Double] = "\\-?[0-9]+\\.?[0-9]*((e|E)-?[0-9]+)?".r ^^ (_.toDouble)
  
  def getVariableId(name: String): Int = {
    val id = variableNameToId.get(name)
    if (!id.isDefined) {
      throw new Exception(s"Unknown variable ?$name in filter!")
    }
    0-id.get // positive ID
  } 
  
  
  val variable: Parser[Var] = {
  "?" ~> identifier ^^ {
    case variableName =>
      val id = getVariableId(variableName)
      Var(id)
   }
  }
  
  val numericLiteral: Parser[NumericLiteral] = {
    double ^^ {
      //case int: Int =>
      //  NumericLiteral(int)
      case double: Double =>
        NumericLiteral(double)
    }
  }
  
  // [55] PrimaryExpression ::= BrackettedExpression | BuiltInCall | IRIrefOrFunction | RDFLiteral | NumericLiteral | BooleanLiteral | Var
  // [56] BrackettedExpression ::= '(' Expression ')'
  // [46] Expression ::= ConditionalOrExpression
  val primaryExpression: Parser[PrimaryExpression] = {
    variable | numericLiteral | ("(" ~> conditionalOrExpression <~ ")")
  }
  
  // [53] MultiplicativeExpression ::= UnaryExpression ( '*' UnaryExpression | '/' UnaryExpression )*
  // [54] UnaryExpression ::= '!' PrimaryExpression | '+' PrimaryExpression | '-' PrimaryExpression | PrimaryExpression
  val multiplicativeExpression: Parser[MultiplicativeExpression] = {
    ("!" | "+" | "-" | "") ~ primaryExpression ~ rep(("*" | "/") ~ ("!" | "+" | "-" | "") ~ primaryExpression) ^^ {
      case prefix ~ lhs ~ otherValues =>
        val entryList = (("", prefix, lhs)) +: otherValues.collect {
           case e => (e._1._1, e._1._2, e._2)
        }
        MultiplicativeExpression(entryList)
    }
  }
  
  // [52] AdditiveExpression ::= MultiplicativeExpression ( '+' MultiplicativeExpression | '-' MultiplicativeExpression | NumericLiteralPositive | NumericLiteralNegative )*
  //! Missing NumericLiteralPositive and -Negative here. !//
  val additiveExpression: Parser[AdditiveExpression] = {
    multiplicativeExpression ~ rep(("+" | "-") ~ multiplicativeExpression) ^^ {
      case lhs ~ otherValues =>
        val entryList = (("", lhs)) +: otherValues.collect {
           case e => (e._1, e._2)
        }
        AdditiveExpression(entryList)
    }
  }
  
  // [50] RelationalExpression ::= NumericExpression ( ('='|'!='|'<'|'>'|'<='|'>=') NumericExpression )?
  // [51] NumericExpression ::= AdditiveExpression
  val relationalExpression: Parser[RelationalExpression] = {
    additiveExpression ~ opt(("=" | "!=" | "<=" | ">=" | "<" | ">") ~ additiveExpression) ^^ {
      case lhs ~ rhs =>
       if (rhs.isDefined) {
         RelationalExpression(lhs, rhs.get._1, Some(rhs.get._2))
       }
       else {
         RelationalExpression(lhs, "", None)
       }
    }
  }
  
  // [48] ConditionalAndExpression ::= ValueLogical ( '&&' ValueLogical )*
  // [49] ValueLogical ::= RelationalExpression
  val conditionalAndExpression: Parser[ConditionalAndExpression] = {
    relationalExpression ~ rep("&&" ~ relationalExpression) ^^ {
      case lhs ~ rhs =>
        val entryList = lhs +: rhs.collect { case e => e._2 }
        ConditionalAndExpression(entryList)
    }
  }
  
  // [47] ConditionalOrExpression ::= ConditionalAndExpression ( '||' ConditionalAndExpression )*
  val conditionalOrExpression: Parser[ConditionalOrExpression] = {
    conditionalAndExpression ~ rep("||" ~ conditionalAndExpression) ^^ {
      case lhs ~ rhs =>
        val entryList = lhs +: rhs.collect { case e => e._2 }
        ConditionalOrExpression(entryList)
    }
  }
  
  // [27] Constraint ::= BrackettedExpression | BuiltInCall | FunctionCall
  // [56] BrackettedExpression ::= '(' Expression ')'
  // [46] Expression ::= ConditionalOrExpression
  val constraint: Parser[Constraint] = {
    conditionalOrExpression
  }
}