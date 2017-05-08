package value

/*
 * Ewok Booles encapsulate Scala Booleans.
 * Couldn't figure out how to call overloaded !
 */
case class Boole(val value: Boolean) extends expression.Literal {
  def &&(other: Boole) = Boole(this.value && other.value)
  def ||(other: Boole) = Boole(this.value || other.value)
  def not = Boole(!this.value)
  override def toString() = "" + value
}