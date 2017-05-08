package expression

import value._
import system.alu

/*
 * Notes:
 * syntax: operator(operands)
 * example: add(mul(2, 3), sub(5, 4), 10)
 * No user-defined functions in Ewok, all functions are implemented in the ALU
 * execute eagerly executes all operands in the first step, even if they aren't all needed.
 */
case class FunCall(val operator: Identifier, val operands: List[Expression]) extends Expression {
  def execute(env: Environment): Value = {
    // use List.map(transformer-function) as a cool way to execute operands 
    val args = operands.map(_.execute(env))
    alu.execute(operator, args)
  }
}