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
  m.isSorted(Array(1,2,3), (x: Int, y: Int) => x <= y)
}

