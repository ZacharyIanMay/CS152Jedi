package expression
import context.Environment
import context.alu
import value.Value
import value.Closure

import scala.::

case class FunCall(val operator: Identifier, val operands: List[Expression]) extends Expression {
  def execute(env: Environment) =
  {
    var ops: List[Value] = List()
    for(e <- operands)
      {
        ops = ops :+ e.execute(env)
      }
    try
    {
      val close = env(operator)
      if(close.isInstanceOf[Closure])
        close.asInstanceOf[Closure].apply(ops)
      else alu.execute(operator, ops)
    } catch
    {
      case e: Exception => alu.execute(operator, ops)
    }
  }
}
