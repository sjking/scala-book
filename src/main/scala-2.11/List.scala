//package fpinscala.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }

  def setHead[A](a: A, as: List[A]): List[A] = as match {
    case Nil => Cons(a, Nil)
    case Cons(_, xs) => Cons(a, xs)
  }

  // List.drop(List(1,2,3,4), 2)
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) =>
      if (n == 0) Cons(x, xs)
      else drop(xs, n-1)
  }

  // List.dropWhile(List(1,2,3,4,5,6,7), (n: Int) => n < 3)
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) =>
      if (f(x)) dropWhile(xs, f)
      else xs
  }

  // val xs: List[Int] = List(1,2,3,4,5)
  // val ex1 = dropWhileCurried(xs)(x => x < 4)
  def dropWhileCurried[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(x, xs)
      if f(x) => dropWhileCurried(xs)(f)
    case _ => l
  }

  // List.init(List(1,2,3,4))
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }
}