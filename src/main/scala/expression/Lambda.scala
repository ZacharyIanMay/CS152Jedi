package expression

import context.Environment
import value.Closure

class Lambda(parameters: List[Identifier] , body: Expression) extends SpecialForm {
  def execute(env: Environment) =
    val c: Closure = Closure(env, body, parameters)
    c
}
