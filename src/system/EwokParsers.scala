package system

import scala.util.parsing.combinator._
import expression._
import value._

/*
 * Notes:
 * disjunction reduces to conjunction reduces to equality ... reduces to term
 * if A reduces to B, then B will have higher precedence than A
 * Example: sum reduces to product, so a + b * c = a + (b * c)
 * Had to make some big corrections to numeral regex
 * This could probably have been a singleton
 * 
 *  expression ::= declaration | conditional | disjunction
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
class EwokParsers extends RegexParsers {

  def expression: Parser[Expression] = declaration | conditional | disjunction | failure("Invalid expression")

  def declaration: Parser[Declaration] = "def" ~ identifier ~ "=" ~ expression ^^ {
    case "def" ~ id ~ "=" ~ exp => Declaration(id, exp)
  }

  def conditional: Parser[Conditional] = "if" ~ "(" ~ expression ~ ")" ~ expression ~ opt("else" ~ expression) ^^ {
    case "if" ~ "(" ~ cond ~ ")" ~ cons ~ None               => Conditional(cond, cons)
    case "if" ~ "(" ~ cond ~ ")" ~ cons ~ Some("else" ~ alt) => Conditional(cond, cons, alt)
  }

  def disjunction: Parser[Expression] = conjunction ~ rep("||" ~> conjunction) ^^ {
    case con ~ Nil  => con
    case con ~ more => Disjunction(con :: more)
  }

  def conjunction: Parser[Expression] = equality ~ rep("&&" ~> equality) ^^ {
    case eq ~ Nil  => eq
    case eq ~ more => Conjunction(eq :: more)
  }

  def equality: Parser[Expression] = inequality ~ rep("==" ~> inequality) ^^ {
    case ineq ~ Nil  => ineq
    case ineq ~ more => FunCall(Identifier("equals"), ineq :: more)
  }

  def inequality: Parser[Expression] = sum ~ opt(("<" | ">" | "!=") ~ sum) ^^ {
    case t ~ None           => t
    case t ~ Some("<" ~ s)  => FunCall(Identifier("less"), List(t, s))
    case t ~ Some(">" ~ s)  => FunCall(Identifier("more"), List(t, s))
    case t ~ Some("!=" ~ s) => FunCall(Identifier("unequals"), List(t, s))
  }

  // exp -> 0 - exp
  def negate(exp: Expression): Expression = {
    val sub = Identifier("sub")
    val zero = Number(0)
    FunCall(sub, List(zero, exp))
  }

  def sum: Parser[Expression] = product ~ rep(("+" | "-") ~ product ^^ {
    case "+" ~ s => s
    case "-" ~ s => negate(s)
  }) ^^ {
    case p ~ Nil  => p
    case p ~ rest => FunCall(Identifier("add"), p :: rest)
  }

  def invert(exp: Expression): Expression = {
    val div = Identifier("div")
    val one = Number(1)
    FunCall(div, List(one, exp))
  }

  def product: Parser[Expression] = term ~ rep(("*" | "/") ~ term ^^ {
    case "*" ~ s => s
    case "/" ~ s => invert(s)
  }) ^^ {
    case p ~ Nil  => p
    case p ~ rest => FunCall(Identifier("mul"), p :: rest)
  }

  def term: Parser[Expression] = lambda | block | funCall | literal | "(" ~> expression <~ ")"

  def literal = boole | numeral | identifier

  def boole: Parser[Boole] = ("true" | "false") ^^ {
    case "true"  => Boole(true)
    case "false" => Boole(false)
  }

  def operands: Parser[List[Expression]] = "(" ~ rep(expression) ~ ")" ^^ {
    case "(" ~ exps ~ ")" => exps
  }

  def funCall: Parser[Expression] = identifier ~ operands ^^ {
    case op ~ ops => FunCall(op, ops)
  }

  def identifier: Parser[Identifier] = """[a-zA-Z][a-zA-Z0-9]*""".r ^^ {
    case someString => Identifier(someString)
  }

  def numeral: Parser[Number] = """(\+|-)?(0|[1-9][0-9]*(.[0-9]+)?)""".r ^^ {
    case digits => Number(digits.toDouble)
  }

  // block ::= "{" ~ expression ~ (";" ~ expression)* ~ "}" ^^ { make a Block}
  def block: Parser[Expression] = "{" ~ expression ~ rep(";" ~ expression ^^ { case ";" ~ exp => exp }) ~ "}" ^^ {
    case "{" ~ exp1 ~ Nil ~ "}"  => Block(List(exp1))
    case "{" ~ exp1 ~ exp2 ~ "}" => Block(exp1 :: exp2)
  }

  // lambda ::= "lambda" ~ parameters ~ expression ^^ {make a Lambda}  

  def lambda: Parser[Expression] = "lambda" ~ parameters ~ expression ^^ {
    case "lambda" ~ params ~ exp => Lambda(params, exp)
  }

  // parameters ::= "(" ~ identifier* ~ ")" ^^ { make List[Identifier] }

  def parameters: Parser[List[Identifier]] = "(" ~> opt(identifier ~ rep("," ~> identifier)) <~ ")" ^^ {
    case None           => Nil
    case Some(e ~ Nil)  => List(e)
    case Some(e ~ exps) => e :: exps
    case _              => Nil
  }

}