package value

import context.TypeException
import expression.Literal

object Boole:
  val TRUE = Boole(true)
  val FALSE = Boole(false)

case class Boole(value: Boolean) extends Literal:
  def &&(other: Value): Boole =
    other match
      case x: Boole => Boole(this.value && x.value)
      case _ => throw new TypeException("Arguments must be a Boole")

  def ||(other: Value): Boole =
    other match
      case x: Boole => Boole(this.value || x.value)
      case _ => throw new TypeException("Arguments must be a Boole")

  def unary_! : Boole =
    Boole(!this.value)

  override def toString() = s"$value"
