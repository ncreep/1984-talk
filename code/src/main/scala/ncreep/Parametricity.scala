package ncreep

import scala.language.higherKinds

object Parametricity {
  def addTwo(i: Int): Int = i + 3

  def addTwo[A](a: A): A = a

  def doSomething(is: List[Int]): List[Int] = ???

  def doSomething2[A](as: List[A]): List[A] = ???

  def swap(x: (Int, Int)): (Int, Int) = (x._1, x._2)

  def swap2[A, B](ab: (A, B)): (B, A) = (ab._2, ab._1)
}