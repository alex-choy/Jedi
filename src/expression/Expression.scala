package expression

import value._

/*
 * Notes:
 * Syntax: expression = literal | funcall | identifier | special-form
 * Expressions are objects that can be executed against an environment 
 * to produce a value.
 */
trait Expression {
  def execute(env: Environment): Value
}