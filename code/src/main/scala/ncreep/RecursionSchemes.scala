package ncreep

import scala.language.higherKinds
import FunctorSyntax._

object RecursionSchemes extends App {

  sealed trait ExpStep[+A] {
    def map[B](f: A => B): ExpStep[B] = this match {
      case AddStep(a, b) => AddStep(f(a), f(b))
      case MulStep(a, b) => MulStep(f(a), f(b))
      case ValStep(x) => ValStep(x)
    }
  }

  case class AddStep[A](a: A, b: A) extends ExpStep[A]
  case class MulStep[A](a: A, b: A) extends ExpStep[A]
  case class ValStep(x: Int) extends ExpStep[Nothing]

  object ExpStep {
    implicit val functor: Functor[ExpStep] = new Functor[ExpStep] {
      def map[A, B](fa: ExpStep[A])(f: A => B): ExpStep[B] = fa.map(f)
    }
  }

  def toExpStep(exp2: Exp2): ExpStep[Exp2] = exp2 match {
    case BinaryOp(Plus, a, b) => AddStep(a, b)
    case BinaryOp(Mult, a, b) => MulStep(a, b)
    case Val2(x: Int) => ValStep(x)
  }

  def evalStep(expStep: ExpStep[Int]): Int = expStep match {
    case AddStep(2, 2) => 5
    case AddStep(a, b) => a + b
    case MulStep(a, b) => a * b
    case ValStep(x) => x
  }

  def fuse[F[_]: Functor, A, B](fromA: A => F[A],
                                toB: F[B] => B)(a: A): B = {
    val fa: F[A] = fromA(a)
    val aToB: A => B = fuse(fromA, toB)
    val fb: F[B] = fa.map(aToB)

    val b: B = toB(fb)
    
    b
  }

  val exp2 = BinaryOp(Mult, Val2(4), BinaryOp(Plus, Val2(5), Val2(1)))
  println(fuse(toExpStep, evalStep)(exp2)) // 24
}