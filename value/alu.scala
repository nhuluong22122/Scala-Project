package value
import expression._
import system._

object alu {
  //dispatcher
  def execute(operator: Identifier, args: List[Value]): Value = {
    operator.name match {
      case "add"    => add(args);
      case "sub"    => sub(args);
      case "mul"    => mul(args);
      case "div"    => div(args);
      case "equals" => equals(args);
      case "less"   => less(args);
      //etc.
      case _        => { new UndefinedException(operator); null }
    }
  }
  //Check for that bads new by checking first -> then continue doing the operation
  private def add(args: List[Value]): Number = {
    var nums = args.filter(_.isInstanceOf[Number])
    if (nums.length != args.length)
      throw new TypeException("Inputs to add must be numbers")
    val nums2 = nums.map(_.asInstanceOf[Number])
    nums2.reduce(_ + _)
  }

  private def sub(args: List[Value]): Number = {
    var nums = args.filter(_.isInstanceOf[Number])
    if (nums.length != args.length)
      throw new TypeException("Inputs to add must be numbers")
    val nums2 = nums.map(_.asInstanceOf[Number])
    nums2.reduce(_ - _)
  }
  private def mul(args: List[Value]): Number = {
    var nums = args.filter(_.isInstanceOf[Number])
    if (nums.length != args.length)
      throw new TypeException("Inputs to add must be numbers")
    val nums2 = nums.map(_.asInstanceOf[Number])
    nums2.reduce(_ * _)
  }

  private def div(args: List[Value]): Number = {
    var nums = args.filter(_.isInstanceOf[Number])
    if (nums.length != args.length)
      throw new TypeException("Inputs to add must be numbers")
    val nums2 = nums.map(_.asInstanceOf[Number])
    nums2.reduce(_ / _)
  }
  private def equals(args: List[Value]): Boole = {
    val no1 = new Number(args(0).toString().toDouble)
    var result = new Boole(false)
    for (arg <- args) {
      result = new Boole(no1.==(new Number(arg.toString().toDouble)))
    }
    result
  }
  private def less(args: List[Value]): Boole = {
    val no1 = new Number(args(0).toString().toDouble)
    val no2 = new Number(args(1).toString().toDouble)
    new Boole(no1.value.<(no2.value))
  }
}