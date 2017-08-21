package value

class Notification(val msg: String) extends Value {
  override def toString = msg
}

object Notification {
  def apply(msg: String) = new Notification(msg)
  val OK = Notification("ok")
  val DONE = Notification("done")
  val UNSPECIFIED = Notification("unspecified")
  //etc. no need to return an error -> throw an exception is better
}