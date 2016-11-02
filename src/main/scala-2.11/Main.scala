/**
  * Created by stephen.king on 2016-01-08.
  */
object Main extends App {
  println("Hello World")
  val m = new MyClass
  println("m.x: " + m.x);


}
object Main2 extends App {
  println("Hello World!")

}

object MainChapter2 extends App {
  val m = new Chapter2
  val ord = (x: Int, y: Int) => x <= y
  val a = m.isSorted(Array(1,2,3), ord)
  val b = m.isSorted(Array(3,2,1), ord)
  println("is sorted")
  println(a, b)

  println("Find first")
  m.findFirst(Array(7,9,13), (x: Int) => x == 9)
}

