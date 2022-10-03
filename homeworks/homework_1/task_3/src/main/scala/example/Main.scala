package example

object Main extends App {
  def hello(helloWord: String, name: String): Unit = {
    println(s"$helloWord Scala! This is $name")
  }

  val myName = "Yulya"
  var reversedName: String = myName.reverse
  val listOfHelloWords: List[String] = List("Hello", "Hola", "Guten tag")

  for (i <- 0 to 2) {
    hello(helloWord = listOfHelloWords(i), name = myName);
    hello(helloWord = listOfHelloWords(i), name = reversedName)
  }
}
