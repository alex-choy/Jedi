package expression
import value._
import system._

case class Iteration(val exp1: Expression, val exp2: Expression) extends SpecialForm {
  def execute(env: Environment): Value = {
    var result: Value = Notification.OK
    var foo = exp1.execute(env)
    if (foo.isInstanceOf[Boole]) 
      while (exp1.execute(env).toString() == "true") 
        result = exp2.execute(env)

    result
  }
}