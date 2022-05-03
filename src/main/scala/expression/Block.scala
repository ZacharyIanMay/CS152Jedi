package expression

import context.Environment
import value.Value

class Block(val exp: List[Expression]) extends SpecialForm {
  def execute(env: Environment) =
    val temp: Environment = new Environment(env)
    var a: Value = null
    for(e <- 0 until exp.size)
      if(e != exp.size-1) exp(e).execute(temp)
      else
        a = exp(e).execute(temp)
    a
}