package com.signalcollect.triplerush.sparql

import scala.util.parsing.combinator.RegexParsers

sealed trait UnaryExpression
case class BuiltInCall(name: String) extends UnaryExpression
case class Var(name: String) extends UnaryExpression
case class NumericLiteral(number: Int) extends UnaryExpression

case class MultiplicativeExpression(value: UnaryExpression)



object FilterParser extends RegexParsers {
    val identifier: Parser[String] = "[-a-zA-Z0-9]*".r
    val integer: Parser[String] = "[0-9]+".r
    
    
    val variable: Parser[Var] = {
    "?" ~> identifier ^^ {
      case variableName =>
        Var(variableName)
     }
    }
    
    val numericLiteral: Parser[NumericLiteral] = {
      integer ^^ {
        case number =>
          NumericLiteral(number.toInt)
      }
    }
    
    val primaryExpression: Parser[UnaryExpression] = {
      variable | numericLiteral
    }
    
    val multiplicativeExpression: Parser[MultiplicativeExpression] = {
      primaryExpression ^^ {
        case value =>
          MultiplicativeExpression(value)
      }
    }
  
}