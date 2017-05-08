package value

/*
 * Notes:
 * Ewok numbers encapsulate Scala Doubles
 * Scala supports operator overloading.
 */
case class Number(val value: Double) extends expression.Literal {
  def +(other: Number) = Number(this.value + other.value)
  def *(other: Number) = Number(this.value * other.value)
  def -(other: Number) = Number(this.value - other.value)
  def /(other: Number) = Number(this.value / other.value)
  def <(other: Number) = new Boole(this.value < other.value)
  def ==(other: Number) = new Boole(this.value == other.value)
  override def toString  = value.toString
}