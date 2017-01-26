package ncreep

sealed trait Except[+Error, +A] {
  def flatMap[B, E >: Error](f: A => Except[E, B]): Except[E, B] = this match {
    case Okay(value) => f(value)
    case Bad(err) => Bad(err)
  }

  def map[B](f: A => B): Except[Error, B] = this match {
    case Okay(value) => Okay(f(value))
    case Bad(err) => Bad(err)
  }
}

case class Okay[+A](value: A) extends Except[Nothing, A]
case class Bad[+Error](error: Error) extends Except[Error, Nothing]

object ExceptionExample extends App {
  def eval(e: Exp): Except[String, Int] = e match {
    case Add(Val(2), Val(2)) => Okay(5)

    case Val(x) => Okay(x)
    case Add(a, b) =>
      for {
        aResult <- eval(a)
        bResult <- eval(b)
      } yield aResult + bResult
    case Mul(_, _) =>
      Bad("We are at war with Multiplication: we had always been at war with Multiplication")
  }

  println(eval(Add(Val(2), Val(2)))) // Okay(5)
  // Bad("We are at war with Multiplication: we had always been at war with Multiplication")
  println(eval(Mul(Val(3), Val(4))))
}