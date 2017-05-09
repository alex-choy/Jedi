package expression

import value._
import system._

/*
 * Notes:
 * syntax: operator(operands)
 * example: add(mul(2, 3), sub(5, 4), 10)
 * No user-defined functions in Ewok, all functions are implemented in the ALU
 * execute eagerly executes all operands in the first step, even if they aren't all needed.
 */
case class FunCall(val operator: Identifier, val operands: List[Expression]) extends Expression {
  /*
  def execute(env: Environment): Value = {
    // use List.map(transformer-function) as a cool way to execute operands 
    val args = operands.map(_.execute(env))
    alu.execute(operator, args)
  }
  * 
  
  
  def execute(env: Environment) {
      val args = operands.map(_.execute(env)) // eager execution
      try {
        val fun = operator.execute(env)
        if (!fun.instanceOf[Closure]) throw new TypeException
        fun.apply(args)
      } catch {
        case e: UndefinedException => alu.execute(operator, args)
      }
      * 
      
}

case class FunCall(val operator: Identifier, val operands: List[Expression] = Nil) extends Expression {
  def execute(env: Environment): Value = {
    val args: List[Value] = result of evaluating operands

  }
}
* 
*/

case class FunCall(val operator: Identifier, val operands: List[Expression]) extends Expression {
   def execute(env: Environment) {
      val args = operands.map(_execute(env)) // eager execution
          //1. check to see if operator is operator.execute(env) is a closure, if so, apply it, if something else, type error
    //2. if undefined exception is thrown and operator is an identifier, catch it and try system.execute
      try {val fun = operator.execute(env)
           if (!fun.instanceOf[Closure]) type error!
           fun.apply(args)
      } catch {
         undefined exception => alu.execute(operator, args)
      }
}