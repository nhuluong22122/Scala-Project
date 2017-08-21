package expression
import value._

trait Literal extends Expression with value.Value {
  def execute(env: Environment) = this
}
//-> 32.execute(env) = 32 (this)