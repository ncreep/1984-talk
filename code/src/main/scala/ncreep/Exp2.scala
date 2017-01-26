package ncreep

import Exp._
import Exp2._

trait Exp2
case class BinaryOp(op: Op, a: Exp2, b: Exp2) extends Exp2
case class Val2(x: Int) extends Exp2

sealed trait Op
case object Plus extends Op
case object Mult extends Op

object Exp2 {
  def toExp(exp2: Exp2): Exp = exp2 match {
    case BinaryOp(Plus, a, b) => Add(toExp(a), toExp(b))
    case BinaryOp(Mult, a, b) => Mul(toExp(a), toExp(b))
    case Val2(x: Int) => Val(x)
  }

  def toExpEvaluate(exp2: Exp2): Int = eval(toExp(exp2))
}

object Examples extends App {
  println(toExp(BinaryOp(Mult, Val2(4), BinaryOp(Plus, Val2(5), Val2(1))))) // Mul(Val(4),Add(Val(5),Val(1)))

  println(toExpEvaluate(BinaryOp(Mult, Val2(4), BinaryOp(Plus, Val2(5), Val2(1))))) // 24

}