package value

class Notification (val msg: String) extends Value {
  
}

object Notification {
  def apply(msg: String) = new Notification(msg)
  val OK = Notification("ok")
  val DONE = Notification("done")
}