package expression
import value._

case class Block(val locals: List[Expression]) extends SpecialForm {
   def execute(env: Environment) = {
      //1. create localEnv extending env
      //2. for local in locals local.execute(localEnv)
      //3. return last one
     val localEnv = new Environment(env)  // 1
     
     for(i <- 0 to locals.length){        // 2
       locals(i).execute(localEnv)
     }
     
     locals.asInstanceOf[Number] // THIS IS A FILLER
   }
}