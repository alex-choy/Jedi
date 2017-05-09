package value
import expression._
import system._

class Closure(val params: List[Identifier], val body: Expression, val defEnv: Environment) extends Value {
   def apply(args: List[Value]): Value = {
      //1. create localEnv extending defEnv // for static scope rule
      //2. bind params to args in localEnv
      //3. body.execute(localEnv)
     
     if(params.size != args.size)
       throw new UndefinedException(params(0))
     var localEnv = new Environment(defEnv)
     for(i <- 0 to params.size){
       localEnv.put(params(i), args(i))
     }
     
     body.execute(localEnv)
   }
}