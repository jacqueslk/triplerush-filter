package com.signalcollect.triplerush.sparql

import scala.util.parsing.combinator.RegexParsers

sealed trait UnaryExpression
case class BuiltInCall(name: String) extends UnaryExpression
case class Var(name: String) extends UnaryExpression
case class NumericLiteral(number: Int) extends UnaryExpression

case class MultiplicativeExpression(firstValue: UnaryExpression, otherValues: Seq[(String, UnaryExpression)])
case class AdditiveExpression(firstValue: MultiplicativeExpression, otherValues: Seq[(String, MultiplicativeExpression)])
case class RelationalExpression(lhs: AdditiveExpression, operator: String, rhs: Option[AdditiveExpression])
case class ConditionalAndExpression(entries: Seq[RelationalExpression])

sealed trait Constraint
case class ConditionalOrExpression(entries: Seq[ConditionalAndExpression]) extends Constraint


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
    
    // [55] PrimaryExpression ::= BrackettedExpression | BuiltInCall | IRIrefOrFunction | RDFLiteral | NumericLiteral | BooleanLiteral | Var
    val primaryExpression: Parser[UnaryExpression] = {
      variable | numericLiteral
    }
    
    // [53] MultiplicativeExpression ::= UnaryExpression ( '*' UnaryExpression | '/' UnaryExpression )*
    // [54] UnaryExpression ::=  '!' PrimaryExpression | '+' PrimaryExpression | '-' PrimaryExpression | PrimaryExpression
    val multiplicativeExpression: Parser[MultiplicativeExpression] = {
      primaryExpression ~ rep(("*" | "/") ~ primaryExpression) ^^ {
        case lhs ~ otherValues =>
          val entryList = scala.collection.mutable.ListBuffer[(String, UnaryExpression)]()
          otherValues.foreach { e=>
            entryList += ((e._1, e._2))
          }
          MultiplicativeExpression(lhs, entryList.toList)
      }
    }
    
    // [52] AdditiveExpression ::= MultiplicativeExpression ( '+' MultiplicativeExpression | '-' MultiplicativeExpression | NumericLiteralPositive | NumericLiteralNegative )*
    val additiveExpression: Parser[AdditiveExpression] = {
      multiplicativeExpression ~ rep(("+" | "-") ~ multiplicativeExpression) ^^ {
        case lhs ~ otherValues =>
          val entryList = scala.collection.mutable.ListBuffer[(String, MultiplicativeExpression)]()
          otherValues.foreach { e=>
            entryList += ((e._1, e._2))
          }
          AdditiveExpression(lhs, entryList.toList)
      }
    }
    
    // [50] RelationalExpression ::= NumericExpression ( ('='|'!='|'<'|'>'|'<='|'>=') NumericExpression )?
    // [51] NumericExpression ::= AdditiveExpression
    val relationalExpression: Parser[RelationalExpression] = {
      additiveExpression ~ opt(("=" | "!=" | "<" | ">" | "<=" | ">=") ~ additiveExpression) ^^ {
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
          val entryList = scala.collection.mutable.ListBuffer[RelationalExpression](lhs)
          rhs.foreach { e=>
            entryList += (e._2)
          }
          ConditionalAndExpression(entryList.toList)
      }
    }
    
    // [47] ConditionalOrExpression ::= ConditionalAndExpression ( '||' ConditionalAndExpression )*
    val conditionalOrExpression: Parser[ConditionalOrExpression] = {
      conditionalAndExpression ~ rep("||" ~ conditionalAndExpression) ^^ {
        case lhs ~ rhs =>
          val entryList = scala.collection.mutable.ListBuffer[ConditionalAndExpression](lhs)
          rhs.foreach { e=>
            entryList += (e._2)
          }
          ConditionalOrExpression(entryList.toList)
      }
    }  
}