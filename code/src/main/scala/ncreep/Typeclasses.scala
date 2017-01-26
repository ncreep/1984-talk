package ncreep
import scala.language.higherKinds

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

trait Applicative[F[_]] extends Functor[F] {
  def unit[A](a: A): F[A]
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]

  def map[A, B](fa: F[A])(f: A => B): F[B] = map2(fa, unit(f)) {
    (a, f) => f(a)
  }
}

trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => flatMap(fb)(b => unit(f(a, b))))
}

object FunctorSyntax {
  implicit class Ops[F[_]: Functor, A](fa: F[A]) {
    def map[B](f: A => B): F[B] = implicitly[Functor[F]].map(fa)(f)
  }
}