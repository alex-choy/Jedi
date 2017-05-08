package expression

import value._

case class Lambda(???)  extends SpecialForm {
   def execute(env: Environment) = {
      new Closure(params, body, env)
   }
}