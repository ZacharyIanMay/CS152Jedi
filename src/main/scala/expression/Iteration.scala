package expression

import context.Environment
import value.Boole
import value.Value

case class Iteration(val condition: Expression, val body: Expression) extends SpecialForm
{
  def execute(env: Environment) =
  {
    var a: Value = null
    var b: Value = condition.execute(env)
    //Include type checking
    while(b.isInstanceOf[Boole] && b.asInstanceOf[Boole] == Boole.TRUE)
        a = body.execute(env)
        b = condition.execute(env)
    a
  }
}
