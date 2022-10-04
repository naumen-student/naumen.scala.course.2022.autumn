package homework_1

object Main  extends App {
  val name = "Andrey Shevchenko"
  val greet = List("Hello", "Hola", "Guten tag")
  def printGreet(l: List[String]): Unit = {
    for (g <- l) println(s"$g Scala! This is $name")
    for (g <- l) println(s"$g Scala! This is ${name.reverse}")
  }
  printGreet(greet)
}
