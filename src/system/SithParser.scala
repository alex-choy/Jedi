/*
package system
import expression._
import value._

class SithParsers extends WookieParsers{
  
  def iteration: Parser[Iteration] = "while"~"("~expression~")"~expression ^^ {
    case "while" ~ "(" ~ exp1 ~ ")" ~ exp2 => new Iteration(exp1, exp2)
  }
  
  def assignment: Parser[Assignment] = identifier~"="~expression ^^ { 
    case id ~ "=" ~ exp => new Assignment(id, exp)
  }
  
  def deref: Parser[Expression] = "["~>expression<~"]" ^^ { 

  }
  

  
  override def expression: Parser[Expression] = declaration | conditional | iteration | assignment | disjunction | failure("Invalid expression")

  override def term: Parser[Expression] = deref | assignment | lambda | block | funCall | literal | "(" ~> expression <~ ")"
}

*/
package system

import expression._
import value._

class SithParsers extends WookieParsers {

  def assignment: Parser[Expression] = identifier ~ "=" ~ expression ^^ {
    case id ~ "=" ~ exp => new Assignment(id, exp)
  }

  def iteration: Parser[Expression] = "while" ~ "(" ~ expression ~ ")" ~ expression ^^ {
    case "while" ~ "(" ~ condition ~ ")" ~ body => new Iteration(condition, body)
  }

  def deref: Parser[Expression] = "[" ~ expression ~ "]" ^^ {
    case "[" ~ exp ~ "]" => FunCall(Identifier("content"), List(exp))
  }

  override def expression: Parser[Expression] = declaration | conditional | iteration | disjunction | failure("Invalid expression")

  override def term: Parser[Expression] = deref | assignment | lambda | block | funCall | literal | "(" ~> expression <~ ")"

}