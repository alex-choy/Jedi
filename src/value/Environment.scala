package value
import scala.collection.mutable.HashMap
import expression._
import system._

class Environment(var extension: Environment = null) extends HashMap[Identifier, Value] with Value {
  override def apply(name: Identifier): Value = {
    if (this.contains(name)) super.apply(name)
    else if (extension != null) extension(name)
    else throw new UndefinedException(name)
  }
}
