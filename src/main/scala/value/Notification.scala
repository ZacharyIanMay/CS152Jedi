package value

class Notification extends Value:
  var note: String = _
  override def toString() = s"$note"

object Notification extends Value:
  def apply(note: String): Notification =
    val n = new Notification
    n.note = note
    n
  val DONE = Notification("Done")
  val OK = Notification("Ok")
  val UNSPECIFIED = Notification("Unspecified")
