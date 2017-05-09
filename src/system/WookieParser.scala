package system
import expression._
import value._

class WookieParsers extends EwokParsers {

   // block ::= "{" ~ expression ~ (";" ~ expression)* ~ "}" ^^ { make a Block}
  def block: Parser[Expression] = "{"~expression~rep(";"~expression ^^ {case ";"~exp => exp})~"}" ^^ {
    case "{" ~ exp1 ~ Nil ~ "}" => Block(List(exp1))
    case "{" ~exp1 ~ exp2 ~ "}" => Block(exp1 :: exp2)
  }


   // lambda ::= "lambda" ~ parameters ~ expression ^^ {make a Lambda}  
  
  def lambda: Parser[Expression] = "lambda" ~ parameters ~ expression ^^ {
    case "lambda" ~ params ~ exp => Lambda(params, exp)
  }
   
   // parameters ::= "(" ~ identifier* ~ ")" ^^ { make List[Identifier] }
  
  def parameters: Parser[List[Identifier]] = "(" ~ opt(identifier ~ rep("," ~ identifier)) <~")" ^^ {
    case Some(id ~ ids) => id :: ids
    case _ => Nil
  }
}