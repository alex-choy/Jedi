package value
import expression._

class Closure(val params: List[Identifier], val body: Expression, val defEnv: Environment) extends Value {
   def apply(args: List[Value]): Value = {
      //1. create localEnv extending defEnv // for static scope rule
      //2. bind params to args in localEnv
      //3. body.execute(localEnv)
     
     params.asInstanceOf[Number] // THIS IS A FILLER
   }
}