package expression
import value._
//def x = 2 + 3 (2 + 3 = body)
case class Declaration(val name : Identifier,val body: Expression) extends SpecialForm with Expression {
  def execute(env: Environment) = { env.put(name, body.execute(env)); Notification.OK}
}