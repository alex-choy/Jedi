package system
import expression._
import value._

class WookieParsers extends EwokParsers {

   // block ::= "{" ~ expression ~ (";" ~ expression)* ~ "}" ^^ { make a Block}

   // lambda ::= "lambda" ~ parameters ~ expression ^^ {make a Lambda}
   
   // parameters ::= "(" ~ identifier* ~ ")" ^^ { make List[Identifier] }
}