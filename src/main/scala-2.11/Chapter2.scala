/**
  * Created by stephen.king on 2016-01-11.
  */

class Chapter2 {
  def fib(n: Int): Int = {
    def fibTail(n1: Int, n2: Int, k: Int): Int =
      if (n == k) n1
      else fibTail(n2, n1+n2, k+1)

    fibTail(0, 1, 1)
  }

  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    def loop(n: Int): Int =
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n + 1)

    loop(0)
  }

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean =
      if (n >= as.length) true
      else if (!ordered(as(n-1), as(n))) false
      else loop(n+1)

    loop(1)
  }

  def partial1[A,B,C](a: A, f: (A,B) => C): B => C =
    (b: B) => f(a,b)

  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a, b)

  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  def compose[A,B,C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))
}

