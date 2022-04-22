package expression
import context.Environment
import value.Boole
import context.TypeException

class Disjunction(operands: List[Expression]) extends SpecialForm {
  def execute(env: Environment) =
  {
    def helper(list: List[Expression]): Boole =
    {
      val head::tail = list
      val h = head.execute(env)
      if(!h.isInstanceOf[Boole]) throw TypeException("Must be a Boole")
      if(h.equals(Boole.TRUE)) Boole.TRUE
      else
      {
        if(tail.isEmpty) Boole.FALSE
        else helper(tail)
      }
    }
    helper(operands)
  }
}
