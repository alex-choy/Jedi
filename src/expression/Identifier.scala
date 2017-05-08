package expression

import value._

/*
 * Notes:
 * syntax: name
 * execute assumes some previous declaration associated name to a value,
 * searches env for this value of name, throws undefined exception if not there.
 */
case class Identifier(val name: String) extends Expression {
  def execute(env: Environment) = env(this) // = env.apply(this)
  override def toString = name
}