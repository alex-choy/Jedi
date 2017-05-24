package system

import expression._
import value._
import system._
import value.Number
import value.Value

/*
 * Notes:
 * alu implements all low-level arithmetic, logic, and I/O functions
 * alu does lots of type checking
 * alu is a singleton
 */
object alu {
  // dispatcher
  def execute(opcode: Identifier, args: List[Value]): Value = {
    opcode.name match {
      case "add"      => add(args)
      case "mul"      => mul(args)
      case "sub"      => sub(args)
      case "div"      => div(args)
      case "less"     => less(args)
      case "more"     => more(args)
      case "equals"   => equals(args)
      case "unequals" => unequals(args)
      case "content"  => content(args)
      case "var"      => makeVar(args)
      // primitive I/O ops:
      case "not"      => not(args)
      case "write"    => write(args)
      case "prompt"   => prompt(args)
      case "read"     => read(args)
      case _          => throw new UndefinedException(opcode)
    }
  }

  private def castAsNumbers(vals: List[Value], opcode: String): List[Number] = {
    if (vals.isEmpty) throw new TypeException(opcode + " expected > 0 inputs")
    val ok = vals.filter(_.isInstanceOf[Number])
    if (ok.length < vals.length) throw new TypeException(opcode + " inputs must be numbers")
    vals.map(_.asInstanceOf[Number])
  }

  private def add(vals: List[Value]): Value = {
    castAsNumbers(vals, "add").reduce(_ + _)
  }

  private def mul(vals: List[Value]): Value = {
    castAsNumbers(vals, "add").reduce(_ * _)
  }

  def sub(vals: List[Value]): Value = {
    castAsNumbers(vals, "add").reduce(_ - _)
  }

  def div(vals: List[Value]): Value = {
    castAsNumbers(vals, "add").reduce(_ / _)
  }

  def less(vals: List[Value]): Value = {
    val args = castAsNumbers(vals, "less")
    if (args.length != 2) throw new TypeException("less inputs must be numbers")
    if (args(0).value < args(1).value) Boole(true) else Boole(false)
  }

  def more(vals: List[Value]): Value = {
    val args = castAsNumbers(vals, "more")
    if (args.length != 2) throw new TypeException("more inputs must be numbers")
    if (args(0).value > args(1).value) Boole(true) else Boole(false)
  }

  def equals(vals: List[Value]): Value = {
    if (vals.isEmpty) throw new TypeException("equals expected > 0 inputs")
    var more = true
    var result = true
    for (i <- 1 until vals.length if more) {
      {
        if (vals(i) != vals(0)) result = false
        more = false
      }
    }
    Boole(result)
  }

  private def content(args: List[Value]): Value = {
    if (args.isEmpty) throw new TypeException("Must have at least 1 arg for content")
    if (args.head.isInstanceOf[Variable]) args.head.asInstanceOf[Variable].content
    else throw new TypeException
  }
  private def makeVar(args: List[Value]) = {
    if (args.isEmpty) throw new TypeException("Must have at least 1 arg to makeVar")
    new Variable(args.head)
  }

  def unequals(vals: List[Value]): Value = {
    if (vals.length != 2) throw new TypeException("unequals expected 2 inputs")
    if (vals(0) != vals(1)) Boole(true) else Boole(false)
  }

  def not(vals: List[Value]): Value = {
    if (vals.length != 1) throw new TypeException("not expected 1 input")
    if (!vals(0).isInstanceOf[Boole]) throw new TypeException("input to not must be Boole")
    (vals(0).asInstanceOf[Boole]).not // can't get Boole.! to work
  }

  def write(vals: List[Value]): Value = { println(vals(0)); Notification.DONE }
  def read(vals: List[Value]): Value = { val result = readDouble(); Number(result) }
  def prompt(vals: List[Value]): Value = { print("=> "); Notification.DONE }

  // etc.
}