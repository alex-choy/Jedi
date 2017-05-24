package expression
import value._

case class Assignment(id: Identifier, exp: Expression) extends SpecialForm {

  def execute(env: Environment): Value = {

    var result = id.execute(env)
    if (result.isInstanceOf[Variable]) {

      result.asInstanceOf[Variable].content = exp.execute(env)

    }
    Notification.DONE
  }
}