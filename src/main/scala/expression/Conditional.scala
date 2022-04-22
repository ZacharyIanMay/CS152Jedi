package expression
import value.Boole
import context.Environment
import value.Notification

class Conditional(condition: Expression, consequent: Expression, alternative: Expression = null) extends SpecialForm {
  override def execute(env: Environment) =
  {
    if(condition.execute(env).equals(Boole.TRUE)) consequent.execute(env)
    else
    {
      if(alternative != null) alternative.execute(env)
      else Notification.UNSPECIFIED
    }
  }
}
