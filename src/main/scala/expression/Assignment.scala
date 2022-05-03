package expression

import context.Environment
import context.TypeException
import value.Notification
import value.Variable

case class Assignment(val vbl: Identifier, val update: Expression) extends SpecialForm
{
  def execute(env: Environment) =
  {
    val v = env(vbl)
    if(!v.isInstanceOf[Variable]) throw TypeException("Input to var must be a value")
    v.asInstanceOf[Variable].content = update.execute(env)
    env(vbl) = v
    Notification.DONE
  }
}
