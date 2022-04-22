package expression
import context.Environment
import value.Notification

class Declaration(identifier: Identifier, expression: Expression) extends SpecialForm
{
  def execute(env: Environment) =
  {
    env(identifier) = expression.execute(env)
    Notification.OK
  }
}
