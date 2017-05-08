package expression
import value._

case class Block(val locals: List[Expression]) extends SpecialForm {
   def execute(env: Environment) = {
      //1. create localEnv extending env
      //2. for local in locals local.execute(localEnv)
      //3. return last one
     
     locals.asInstanceOf[Number] // THIS IS A FILLER
   }
}