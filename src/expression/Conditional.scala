package expression

import value._
import system._

/*
 * Notes:
 * syntax: if (condition) consequent (else alternative)?
 * execute lazily ignores either consequent or alternative depending on condition.
 * execute basically reduces to Scala's if-else
 * if (false) consequent = unspecified
 * 
 */
case class Conditional(val condition: Expression, val consequent: Expression, val alternative: Expression = null) extends SpecialForm {
   def execute(env: Environment) = {
     val cond = condition.execute(env)
     if (!cond.isInstanceOf[value.Boole]) 
       throw new TypeException("If condition must be Boole")
     if (cond.asInstanceOf[value.Boole].value) {
       consequent.execute(env)
     } else if (alternative != null) {
       alternative.execute(env)
     } else {
       Notification("unspecified")
     }
   }
}