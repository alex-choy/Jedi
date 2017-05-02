package expression
import value._
import system._

case class Conjunction(val exps: List[Expression]) extends SpecialForm {
  def execute(env: Environment) {
    var more = true
    var result = Boole(false)
    for (exp <- exps if more) {
      val arg = exp.execute(env)
      if (!arg.isInstanceOf[Boole]) throw new TypeException(arg.toString)
      val b = arg.asInstanceOf[Boole]
      if (!b.value) {
        result = Boole(true)
        more = false
      }
      result
    }
  }
}