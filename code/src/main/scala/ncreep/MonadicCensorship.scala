package ncreep

object MonadicCensorship extends App {
  type Censor = Exp => Except[String, Exp]

  val rule: Censor = exp => exp match {
    case Val(4) => Okay(Val(5))
    case Val(7) => Bad("7 is an unperson")
    case Add(Val(9), _) | Add(_, Val(9)) => Bad("Adding 9 is a thoughtcrime")
    case Mul(Val(3), _) | Mul(_, Val(3)) => Bad("Multiplication by 3 is double plus ungood")

    case Add(a, b) => for {
      censoredA <- rule(a)
      censoredB <- rule(b)
    } yield Add(censoredA, censoredB)

    case Mul(a, b) => for {
      censoredA <- rule(a)
      censoredB <- rule(b)
    } yield Mul(censoredA, censoredB)

    case Val(x) => Okay(Val(x))
  }

println(rule(Add(Val(4), Val(6)))) // Okay(Add(Val(5),Val(6)))

println(rule(Add(Val(7), Mul(Mul(Val(3), Val(5)), Add(Val(9), Val(2)))))) // Bad(7 is an unperson)
}

object MonadicCensorship2 extends App {
  type Censor = Exp => Except[List[String], Exp]

  val rule: Censor = exp => exp match {
    case Val(4) => Okay(Val(5))
    case Val(7) => Bad(List("7 is an unperson"))
    case Add(Val(9), _) | Add(_, Val(9)) => Bad(List("Adding 9 is a thoughtcrime"))
    case Mul(Val(3), _) | Mul(_, Val(3)) => Bad(List("Multiplication by 3 is double plus ungood"))

    case Add(a, b) => for {
      censoredA <- rule(a)
      censoredB <- rule(b)
    } yield Add(censoredA, censoredB)

    case Mul(a, b) => for {
      censoredA <- rule(a)
      censoredB <- rule(b)
    } yield Mul(censoredA, censoredB)

    case Val(x) => Okay(Val(x))
  }

  println(rule(Add(Val(4), Val(6)))) // Okay(Add(Val(5),Val(6)))

  println(rule(Add(Val(7), Mul(Mul(Val(3), Val(5)), Add(Val(9), Val(2)))))) // Bad(List(7 is an unperson))
}

object MonadicCensorship3 extends App {
  type Censor = Exp => Except[List[String], Exp]

  val rule: Censor = exp => exp match {
    case Val(4) => Okay(Val(5))
    case Val(7) => Bad(List("7 is an unperson"))
    case Add(Val(9), _) | Add(_, Val(9)) => Bad(List("Adding 9 is a thoughtcrime"))
    case Mul(Val(3), _) | Mul(_, Val(3)) => Bad(List("Multiplication by 3 is double plus ungood"))

    case Add(a, b) =>
      rule(a).flatMap { censoredA =>
        rule(b).map { censoredB =>
          Add(censoredA, censoredB)
        }
      }

    case Mul(a, b) =>
      rule(a).flatMap { censoredA =>
        rule(b).map { censoredB =>
          Mul(censoredA, censoredB)
        }
      }

    case Val(x) => Okay(Val(x))
  }

  println(rule(Add(Val(4), Val(6)))) // Okay(Add(Val(5),Val(6)))

  println(rule(Add(Val(7), Mul(Mul(Val(3), Val(5)), Add(Val(9), Val(2)))))) // Bad(List(7 is an unperson))
}