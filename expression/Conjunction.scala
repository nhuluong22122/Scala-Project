package expression
import value._
import system._
case class Conjunction(val exps: List[Expression]) extends SpecialForm {
  // one or more equality separated by the && sign
  def execute(env: Environment) = {
    var more = true
    var result = new Boole(true)
    for (exp <- exps if more) {
      val arg = exp.execute(env)
      if (!arg.isInstanceOf[Boole]) throw new TypeException("Please enter a boolean")
      val b = arg.asInstanceOf[Boole]
      if (!b.value) {
        result = new Boole(false)
        more = false
      }
    }
    result
  }
}