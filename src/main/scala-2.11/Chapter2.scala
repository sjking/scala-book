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
}
