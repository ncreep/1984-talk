package ncreep

sealed trait Exp

case class Add(a: Exp, b: Exp) extends Exp
case class Mul(a: Exp, b: Exp) extends Exp
case class Val(x: Int) extends Exp

object Exp {
  def eval(e: Exp): Int = e match {
    case Add(Val(2), Val(2)) => 5
    case Add(a, b) => eval(a) + eval(b)
    case Mul(a, b) => eval(a) * eval(b)
    case Val(x) => x
  }

  def print(e: Exp): String = e match {
    case Add(Val(2), Val(2)) => "5"
    case Add(a, b) => s"${print(a)} + ${print(b)}"
    case Mul(a, b) => s"${print(a)} * ${print(b)}"
    case Val(x) => x.toString
  }
}

object Example extends App {
  import Exp._
  
  println(eval(Add(Val(3), Mul(Val(2), Val(5))))) // 13
  println(eval(Add(Val(2), Val(2)))) // 5
  
  println(print(Add(Val(3), Mul(Val(2), Val(5))))) // 3 + 2 * 5
  println(print(Add(Val(2), Val(2)))) // 5
}