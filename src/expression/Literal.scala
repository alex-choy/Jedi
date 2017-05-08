package expression

import value._

/*
 * Notes:
 * syntax: literal = number | true | false
 * literal expressions are their own values
 */
trait Literal extends Expression with Value {
  def execute(env: Environment) = this
}