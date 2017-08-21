package expression
import value._
case class Conditional(c: Expression, i: Expression, e: Expression) extends SpecialForm {
  def execute(env: Environment) = {
    if (e == null) {
        Notification.UNSPECIFIED;
    } else {
      if (c.execute(env) == new Boole(true)) i.execute(env)
      else e.execute(env)
    }
  }
}