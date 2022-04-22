package expression
import context.Environment
import context.alu
import value.Value

import scala.::

class FunCall(operator: Identifier, operands: List[Expression]) extends Expression {
  def execute(env: Environment) =
  {
    var ops: List[Value] = List()
    for(e <- operands)
      {
        ops = ops :+ e.execute(env)
      }
    alu.execute(operator, ops)
  }
}
