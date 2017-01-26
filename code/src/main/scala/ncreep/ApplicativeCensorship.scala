package ncreep

sealed trait Censorship[+A] {
  def map2[B, C](fb: Censorship[B])(f: (A, B) => C): Censorship[C] = 
    Censorship.map2(this, fb)(f)
}

object Censorship {
  def map2[A, B, C](fa: Censorship[A], fb: Censorship[B])(f: (A, B) => C): Censorship[C] =
    (fa, fb) match {
      case (Pass(a), Pass(b)) => Pass(f(a, b))
      case (Fail(errors), Pass(_)) => Fail(errors)
      case (Pass(_), Fail(errors)) => Fail(errors)
      case (Fail(errors1), Fail(errors2)) => Fail(errors1 ++ errors2)
    }
}

case class Pass[+A](value: A) extends Censorship[A]
case class Fail(errors: List[String]) extends Censorship[Nothing]

object Fail {
  def apply(error: String): Fail = Fail(List(error))
}

object ApplicativeCensorship extends App {
  type Censor = Exp => Censorship[Exp]

  val rule: Censor = exp => exp match {
    case Val(4) => Pass(Val(5))
    case Val(7) => Fail("7 is an unperson")
    case Add(Val(9), _) | Add(_, Val(9)) => Fail("Adding 9 is a thoughtcrime")
    case Mul(Val(3), _) | Mul(_, Val(3)) => Fail("Multiplication by 3 is double plus ungood")

    case Add(a, b) =>
      rule(a).map2(rule(b)) { (censoredA, censoredB) =>
        Add(censoredA, censoredB)
      }

    case Mul(a, b) =>
      rule(a).map2(rule(b)) { (censoredA, censoredB) =>
        Mul(censoredA, censoredB)
      }

    case Val(x) => Pass(Val(x))
  }

  println(rule(Add(Val(4), Val(6)))) // Pass(Add(Val(5),Val(6)))

  // Fail(List(7 is an unperson, Multiplication by 3 is double plus ungood, Adding 9 is a thoughtcrime))
  println(rule(Add(Val(7), Mul(Mul(Val(3), Val(5)), Add(Val(9), Val(2))))))
}