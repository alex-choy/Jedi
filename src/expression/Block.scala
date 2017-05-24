package expression
import value._

case class Block(val locals: List[Expression]) extends SpecialForm {
  def execute(env: Environment) = {
    val localEnv = new Environment(env)
    var last: Value = Notification.UNSPECIFIED
    for(i <- 0 to locals.size - 1) {
      last = locals(i).execute(localEnv)
    }
    last
  }
}