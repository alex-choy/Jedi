package expression
import value._

case class FunCall(val operator: Identifier, val operands: List[Expression]) extends Expression {
  def execute(env: Environment): Value = {
    val args = operands.map(_.execute(env))
    alu.execute(operator, args)
  }
}