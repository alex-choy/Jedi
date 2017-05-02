package expression
import value._

case class Identifier (val name: String) extends Expression {
  def execute (env: Environment) = env(this)
}