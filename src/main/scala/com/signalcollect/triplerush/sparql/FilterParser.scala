package com.signalcollect.triplerush.sparql

import scala.util.parsing.combinator.RegexParsers

// Filter Triple case classes / traits
import com.signalcollect.triplerush.UnaryExpression
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
      primaryExpression ~ rep(("/" | "*") ~ primaryExpression) ^^ {
        case lhs ~ otherValues =>
          val entryList = (("", lhs)) +: otherValues.collect {
             case e => (e._1, e._2)
          }
          MultiplicativeExpression(entryList)
      }
    }
    
    // [52] AdditiveExpression ::= MultiplicativeExpression ( '+' MultiplicativeExpression | '-' MultiplicativeExpression | NumericLiteralPositive | NumericLiteralNegative )*
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
}