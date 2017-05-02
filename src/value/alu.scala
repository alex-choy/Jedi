package value
import expression.Identifier
import system._

object alu {
  // dispatcher
  def execute(operator: Identifier, args: List[Value]): Value = {
    operator.name match {
      case "add"   => add(args)
      case "sub"   => sub(args)
      case "mul"   => mul(args)
      case "div"   => div(args)
      case "equal" => equal(args)
      // etc
      case _       => throw new UndefinedException(operator)
    }
  }

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
      throw new TypeException("Inputs to subtract must be numbers")
    val nums2 = nums.map(_.asInstanceOf[Number])
    nums2.reduce(_ - _)
  }

  private def mul(args: List[Value]): Number = {
    var nums = args.filter(_.isInstanceOf[Number])
    if (nums.length != args.length)
      throw new TypeException("Inputs to multiply must be numbers")
    val nums2 = nums.map(_.asInstanceOf[Number])
    nums2.reduce(_ * _)
  }

  private def div(args: List[Value]): Number = {
    var nums = args.filter(_.isInstanceOf[Number])
    if (nums.length != args.length)
      throw new TypeException("Inputs to divide must be numbers")
    val nums2 = nums.map(_.asInstanceOf[Number])
    nums2.reduce(_ / _)
  }

  def equal(args: List[Value]): Boole = {
    var result = true
    for (i <- 0 until args.length - 1) {
      var param1 = args(i)
      var param2 = args(i + 1)
      if (param1.isInstanceOf[Number] && param2.isInstanceOf[Number]) {
        var param3 = param1.asInstanceOf[Number]
        var param4 = param2.asInstanceOf[Number]
        result = result && param3.value == param4.value
      } else if (param1.isInstanceOf[Boole] && param2.isInstanceOf[Boole]) {
        var param3 = param1.asInstanceOf[Boole]
        var param4 = param2.asInstanceOf[Boole]
        result = result && param3.value == param4.value
      } else
        result = false

    }
    result.asInstanceOf[Boole]
  }

  // etc.
}