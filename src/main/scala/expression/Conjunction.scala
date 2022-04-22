package expression
import context.Environment
import value.Boole
import context.TypeException

class Conjunction(operands: List[Expression]) extends SpecialForm {
  def execute(env: Environment) =
  {
    def helper(list: List[Expression]): Boole =
    {
      val head::tail = list
      if(!head.isInstanceOf[Boole]) throw TypeException("Must be a Boole")
      if(head.equals(Boole.FALSE)) Boole.FALSE
      else
      {
        if(tail.isEmpty) Boole.TRUE
        else helper(tail)
      }
    }
    helper(operands)
  }
}
