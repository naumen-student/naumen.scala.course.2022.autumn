package example

object Main extends App{
  val myName = "Katya Valik"
  val hello: List[String] = List("Hello", "Hola", "Guten tag")
  hello.foreach(s => println(s"$s  Scala! This is $myName"))
  hello.foreach(s => println(s"$s  Scala! This is ${myName.reverse}"))
}
