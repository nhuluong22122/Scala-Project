package expression
import value._
import system._
case class Disjunction(val exps: List[Expression]) extends SpecialForm {
  def execute(env: Environment) = {
    var more = true
    var result = new Boole(false)
    for (exp <- exps if more) {
      val arg = exp.execute(env)
      if (!arg.isInstanceOf[Boole]) throw new TypeException("Please enter a boolean")
      val b = arg.asInstanceOf[Boole]
      if (b.value) {
        result = new Boole(true)
        more = false
      }
    }
    result
  }
}