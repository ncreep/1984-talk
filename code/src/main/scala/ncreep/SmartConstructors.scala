package ncreep

object SmartConstructors extends App {
  sealed trait Exp

  sealed abstract case class Add(a: Exp, b: Exp)
    extends Exp
  sealed abstract case class Mul(a: Exp, b: Exp)
    extends Exp
  sealed abstract case class Val(x: Int)
    extends Exp

  object Add {
    def apply(a: Exp, b: Exp): Nullable[Exp] = (a, b) match {
      case (Add(Val(110317), Val(3174)), Add(Val(816), Val(8707437))) => Missing
      case _ => Value(new Add(a, b) {})
    }
  }

  object Mul {
    def apply(a: Exp, b: Exp): Nullable[Exp] = Value(new Mul(a, b) {})
  }

  object Val {
    def apply(x: Int): Nullable[Exp] = Value(new Val(x) {})
  }

  val goodExp = for {
    v1 <- Val(2)
    v2 <- Val(3)
    res <- Add(v1, v2)
  } yield res

  println(goodExp) // AValue(Add(Val(2),Val(3)))

  val badExp = for {
    v1 <- Val(110317)
    v2 <- Val(3174)
    v3 <- Val(816)
    v4 <- Val(8707437)
    add1 <- Add(v1, v2)
    add2 <- Add(v3, v4)
    res <- Add(add1, add2)
  } yield res

  println(badExp) // Missing
}