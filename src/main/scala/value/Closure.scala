package value

import context.Environment
import expression.Identifier
import expression.Expression

class Closure(defEnv: Environment, body: Expression , parameters: List[Identifier]) extends Value {
  def apply(args: List[Value]) =
    val temp = new Environment(defEnv)
    temp.bulkPut(parameters, args)
    body.execute(temp)
}
