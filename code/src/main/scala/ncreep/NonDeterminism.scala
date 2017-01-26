package ncreep

object NonDeterminism extends App {
  def eval(e: Exp): List[Int] = e match {
    case Add(Val(2), Val(2)) => List(3, 5)

    case Val(x) => List(x)
    case Add(a, b) =>
      for {
        aResult <- eval(a)
        bResult <- eval(b)
      } yield aResult + bResult
    case Mul(a, b) =>
      for {
        aResult <- eval(a)
        bResult <- eval(b)
      } yield aResult * bResult
  }

  println(eval(Mul(Add(Val(2), Val(2)), Add(Val(3), Add(Val(2), Val(2)))))) // List(18, 24, 30, 40)
}