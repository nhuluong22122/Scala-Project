package expression
import value._
//case class FunCall(operator: Identifier, operands: List[Expression]) extends Expression {
//  def execute(env: Environment) = {
//    var args: List[Value] = List()
//    for(operand <- operands){
//      val arg = operand.execute(env)
//      args = arg::args
//    }
//    alu.execute(operator, args)
//  }
//  
//}

case class FunCall(val operator: Identifier, val operands: List[Expression]) extends Expression {
  def execute(env: Environment): Value = {
    val listofValue = operands.map(_.execute(env))
    alu.execute(operator, listofValue)
  }
}