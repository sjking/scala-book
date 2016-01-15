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

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sum2(ns: List[Int]) = foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)

  def length[A](as: List[A]): Int = foldRight[A, Int](as, 0)((_,y) => y + 1)

  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
    @annotation.tailrec
    def foldl(xs: List[A], acc: B): B = xs match {
      case Nil => acc
      case Cons(x, xs) => foldl(xs, f(acc, x))
    }
    foldl(as, z)
  }

  def sumLeft(ns: List[Int]) = foldLeft(ns, 0)(_ + _)
  def productLeft(ns: List[Double]) = foldLeft(ns, 1.0)(_ * _)
  def lengthLeft[A](ns: List[A]): Int = foldLeft[A, Int](ns, 0)((y,_) => y + 1)

  def reverseList[A](ns: List[A]) = foldLeft(ns, Nil:List[A])((b, a) => Cons(a, b))

  def append[A](as: List[A], bs: List[A]) = foldRight(as, bs)(Cons(_,_))

  def flattenLists[A](ls: List[List[A]]): List[A] = foldLeft(ls, Nil:List[A])((x,y) => append(x,y)) // (append(_,_)) but scala complained

//  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
//    (a: A) => (b: B) => f(a, b)

//  def foldRightOn[A,B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(as, z)((xs, x) => curry[A,B,B](f(x, xs)))

  // List.map(List(1,2,3))(_*2)
  def map[A,B](as: List[A])(f: A => B): List[B] = foldRight(as, Nil:List[B])((x,y) => Cons(f(x), y))

  // List.filter(List(1,2,3,1,5,6,7,1,9))(_!=1)
  def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(x, xs) =>
      if (f(x)) Cons(x, filter(xs)(f))
      else filter(xs)(f)
  }

  // flatMap(List(1,2,3))(i => List(i,i))
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = flattenLists(foldLeft(as, Nil:List[List[B]])((b,a) => Cons(f(a), b)))

}