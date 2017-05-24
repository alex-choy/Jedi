package system
import expression._
import value._

class WookieParsers extends EwokParsers {
  // block ::= "{" ~ expression ~ (";" ~ expression)* ~ "}" ^^ { make a Block}
  def block: Parser[Expression] = "{" ~ expression ~ rep(";" ~ expression ^^ { case ";" ~ exp => exp }) ~ "}" ^^ {
    case "{" ~ exp1 ~ Nil ~ "}"  => Block(List(exp1))
    case "{" ~ exp1 ~ exp2 ~ "}" => Block(exp1 :: exp2)
    case _                       => throw new Exception()
  }

  // lambda ::= "lambda" ~ parameters ~ expression ^^ {make a Lambda}  
  def lambda: Parser[Expression] = "lambda" ~ parameters ~ expression ^^ {
    case "lambda" ~ params ~ exp => Lambda(params, exp)
    case _                       => throw new UndefinedException(Identifier("Wrong format for Lambda"))
  }

  // parameters ::= "(" ~ identifier* ~ ")" ^^ { make List[Identifier] }

  def parameters: Parser[List[Identifier]] = "(" ~> opt(identifier ~ rep("," ~> identifier)) <~ ")" ^^ {
    case None           => Nil
    case Some(e ~ Nil)  => List(e)
    case Some(e ~ exps) => e :: exps
    case _              => Nil
  }

  override def term: Parser[Expression] = lambda | block | funCall | literal | "(" ~> expression <~ ")"
}

