package value

import context.TypeException

case class Chars(value: String) extends Addable with Ordered[Value]:
  def size(): Exact = Exact(value.size)

  def subChars(from: Exact, to: Exact): Chars =
    Chars(value.substring(from.value, to.value))

  def subChars(from: Int, to: Int): Chars =
    Chars(value.substring(from, to))

  def +(other: Value): Chars =
    other match
      case x: Chars => Chars(this.value + x.value)
      case x: Exact => Chars(this.value + x.value)
      case x: Inexact => Chars(this.value + x.value)
      case _ => throw new TypeException("Arguments must be addable")

  def compare(other: Value): Int =
    other match
      case x: Exact => this.value.compare(x.value.toString)
      case x: Inexact => this.value.compare(x.value.toString)
      case x: Chars => this.value.compare(x.value)
      case _ => throw new TypeException("Arguments must be comparable")

  override def toString() = s"$value"
