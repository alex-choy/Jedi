package value

case class Boole(val value: Boolean) extends expression.Literal {
  def &&(other: Boole) = new Boole(this.value && other.value)
  // etc.
}