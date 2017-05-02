package expression
import value._
import system._

case class Conditional(condition: Expression, result1: Expression, result2: Expression = null) extends SpecialForm {
  def execute(env: Environment) {
    val value = condition.execute(env)
    if (!value.isInstanceOf[Boole]) throw new TypeException(value.toString)
    val valueData = value.asInstanceOf[Boole].value
    if (valueData) result1.execute(env)
    else if (null != result2) result2.execute(env)
    else throw new TypeException("Unexcpected result from Conditional")
  }
}
