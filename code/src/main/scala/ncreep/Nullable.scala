package ncreep

sealed trait Nullable[+A] {
  def chainNullable[B](f: A => Nullable[B]): Nullable[B] = this match {
    case Value(a) => f(a)
    case Missing => Missing
  }

  def flatMap[B](f: A => Nullable[B]): Nullable[B] = chainNullable(f)

  def map[B](f: A => B): Nullable[B] = this match {
    case Value(a) => Value(f(a))
    case Missing => Missing
  }
}

case class Value[A](value: A) extends Nullable[A]
case object Missing extends Nullable[Nothing]

object NullExample extends App {
  def eval(e: Exp): Integer = e match {
    case Val(7) => null
    case Val(x) => x

    case Add(Val(2), Val(2)) => 5
    case Add(a, b) =>
      val aResult = eval(a)
      if (aResult == null) null
      else {
        val bResult = eval(b)
        if (bResult == null) null
        else aResult + bResult
      }
    case Mul(a, b) =>
      val aResult = eval(a)
      if (aResult == null) null
      else {
        val bResult = eval(b)
        if (bResult == null) null
        else aResult * bResult
      }
  }

  println(eval(Add(Val(2), Val(2)))) // 5
  println(eval(Add(Val(3), Val(7)))) // null
}

object SimpleNullableExample extends App {
  def eval(e: Exp): Nullable[Int] = e match {
    case Val(7) => Missing
    case Val(x) => Value(x)

    case Add(Val(2), Val(2)) => Value(5)
    case Add(a, b) =>
      eval(a) match {
        case Value(aResult) =>
          eval(b) match {
            case Value(bResult) => Value(aResult + bResult)
            case Missing => Missing
          }
        case Missing => Missing
      }
    case Mul(a, b) =>
      eval(a) match {
        case Value(aResult) =>
          eval(b) match {
            case Value(bResult) => Value(aResult * bResult)
            case Missing => Missing
          }
        case Missing => Missing
      }
  }

  println(eval(Add(Val(2), Val(2)))) // Value(5)
  println(eval(Add(Val(3), Val(7)))) // Missing
}

object HigherOrderFunctionNullable extends App {
  def eval(e: Exp): Nullable[Int] = e match {
    case Val(7) => Missing
    case Val(x) => Value(x)

    case Add(Val(2), Val(2)) => Value(5)
    case Add(a, b) =>
      eval(a).chainNullable { aResult =>
        eval(b).chainNullable { bResult =>
          Value(aResult + bResult)
        }
      }
    case Mul(a, b) =>
      eval(a).chainNullable { aResult =>
        eval(b).chainNullable { bResult =>
          Value(aResult * bResult)
        }
      }
  }

  println(eval(Add(Val(2), Val(2)))) // Value(5)
  println(eval(Add(Val(3), Val(7)))) // Missing
}

object ComprehensionNullable extends App {
  def eval(e: Exp): Nullable[Int] = e match {
    case Val(7) => Missing
    case Val(x) => Value(x)
    case Add(Val(2), Val(2)) => Value(5)
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

  println(eval(Add(Val(2), Val(2)))) // Value(5)
  println(eval(Add(Val(3), Val(7)))) // Missing
}