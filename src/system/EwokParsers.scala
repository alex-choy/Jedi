package system

import scala.util.parsing.combinator._
import expression._
import value._

class EwokParsers extends RegexParsers {

  /* 
		Ewok grammar:

   expression ::= declaration | conditional | disjunction
   declaration ::= "def"~identifier~"="~expression
   conditional ::= "if"~"("~expression~")"~expression~("else"~expression)?
   disjunction ::= conjunction~("||"~conjunction)*
   conjunction ::= equality~("&&"~equality)*
   equality ::= inequality~("=="~inequality)*
   inequality ::= sum ~ ("<" ~ sum)?
   sum ::= product ~ (("+" | "-") ~ product)*
   product ::= term ~ (("*" | "/") ~ term)*
   term ::= funcall | identifier | number | boole | "("~expression~")"
   funcall ::= identifier~operands
   operands ::= "("~(expression ~ (","~expression)*)? ~ ")"
   identifier ::= [a-zA-Z][a-zA-Z0-9]*
   number ::= [1-9][0-9]*("."[0-9]+)?
   boole ::= "true" | "false"
   
*/

  def boole: Parser[Boole] = """true|false""".r ^^ {
    case exp => Boole(exp.toBoolean)
  }

  def conditional: Parser[Conditional] = "if" ~ "(" ~ expression ~ ")" ~ expression ~ opt("else" ~ expression) ^^ {
    case "if" ~ "(" ~ condition ~ ")" ~ result1 ~ Some("else" ~ result2) => Conditional(condition, result1, result2)
    case "if" ~ "(" ~ condition ~ ")" ~ result1 ~ None                   => Conditional(condition, result1)
  }

  def conjunction: Parser[Expression] = equality ~ rep("&&" ~> equality) ^^ {
    case cond ~ Nil   => cond
    case cond ~ conds => Conjunction(cond :: conds)
  }

  def declaration: Parser[Declaration] = "def" ~ identifier ~ "=" ~ expression ^^ {
    case "def" ~ id ~ "=" ~ exp => Declaration(id, exp)
  }

  def disjunction: Parser[Expression] = conjunction ~ rep("||" ~> conjunction) ^^ {
    case con ~ Nil  => con
    case con ~ cons => Disjunction(con :: cons)
  }

  def divide(exp: Expression): Expression = {
    val div = Identifier("div")
    val one = Number(1.0)
    FunCall(div, List(one, exp))
  }

  def equality: Parser[Expression] = inequality ~ rep("==" ~> inequality) ^^ {
    case unequal ~ Nil      => unequal
    case unequal ~ unequals => FunCall(Identifier("equals"), unequal :: unequals)
  }

  def expression: Parser[Expression] = (declaration | conditional | disjunction | failure("Invalid expression"))

  def funcall: Parser[Expression] = term ~ opt(operands) ^^ {
    case term ~ None   => term
    case t ~ Some(Nil) => FunCall(t.asInstanceOf[Identifier], Nil)
    case t ~ Some(ops) => FunCall(t.asInstanceOf[Identifier], ops)
  }

  def identifier: Parser[Identifier] = """[a-zA-Z][a-zA-Z0-9]*""".r ^^ {
    case someString => Identifier(someString)
  }

  def inequality: Parser[Expression] = sum ~ rep("<" ~> sum) ^^ {
    case unequal ~ Nil         => unequal
    case unequal ~ unequalList => FunCall(Identifier("less"), unequal :: unequalList)
  }

  def literal: Parser[Literal] = boole | number

  def negate(exp: Expression): Expression = {
    val sub = Identifier("sub")
    val zero = Number(0.0)
    FunCall(sub, List(zero, exp))
  }

  def number: Parser[Number] = """[1-9][0-9]*(","[0-9]+)?""".r ^^ {
    case someString => Number(someString.toDouble)
  }

  def operands: Parser[List[Expression]] = "(" ~> opt(expression ~ rep("," ~> expression)) <~ ")" ^^ {
    case None             => Nil
    case Some(exp ~ Nil)  => List(exp)
    case Some(exp ~ exps) => exp :: exps
    case _                => Nil
  }

  def product: Parser[Expression] =
    funcall ~ rep("""\*|/""".r ~ funcall ^^ { case "*" ~ s => s case "/" ~ s => divide(s) }) ^^ {
      case operand ~ Nil       => operand
      case operand1 ~ operand2 => FunCall(Identifier("mul"), operand1 :: operand2)
    }

  def sum: Parser[Expression] =
    product ~ rep(("+" | "-") ~ product ^^ { case "+" ~ s => s case "-" ~ s => negate(s) }) ^^ {
      case p ~ Nil  => p
      case p ~ rest => FunCall(Identifier("add"), p :: rest)
    }

  def term: Parser[Expression] = (funcall | identifier | literal | "(" ~> expression <~ ")")

}