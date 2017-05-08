package expression

import value._
import system._

/*
 * Notes:
 * syntax: exp-1 && exp-2 && ... && exp-N
 * execute implements short circuit execution, uses more flag
 * to halt execution when false is found.
 */
case class Conjunction(val exps:List[Expression]) extends Expression {
  def execute(env: Environment) = {
    var more = true
    var result = Boole(true)
    for(exp <- exps if more) {
      val arg = exp.execute(env)
      if (!arg.isInstanceOf[value.Boole]) 
        throw new TypeException("Conjunction inputs must be Booles")
      val b = arg.asInstanceOf[value.Boole]
      if (!b.value) {
        result = Boole(false)
        more = false
      }
    }
    result
  }
  
}