package context

import scala.util.parsing.combinator._
import expression._
import value._

class Jedi3Parsers extends Jedi2Parsers {

  // assignment ::= identifier ~ ":=" ~ expression
  def assignment : Parser[Assignment] = identifier ~ ":=" ~ expression ^^ {
    case identifier ~ ":=" ~ expression => Assignment(identifier, expression)
  }

  // iteration ::= "while" ~ "(" ~ expression ~ ")" ~ expression
  def iteration : Parser[Iteration] = "while" ~ "(" ~ expression ~ ")" ~ expression ^^ {
    case "while" ~ "(" ~ con ~ ")" ~ exp => Iteration(con, exp)
  }

  // dereference ::= "[" ~ expression ~ "]"
  def dereference : Parser[Expression] = "[" ~> expression <~ "]" ^^ {
    case expression => FunCall(Identifier("dereference"), List(expression))
  }


  override def expression: Parser[Expression] = declaration | conditional | iteration | disjunction | failure("Invalid expression")
  override def term: Parser[Expression]  = lambda | funCall | block |  assignment | dereference | literal | "("~>expression<~")"

}
