package example

object Main extends App {
  val greetings = Array("Hola", "Guten tag", "Hello")
  val text = "Scala! This is"
  val name = "xsitin"
  for (greeting <- greetings)
    hello(greeting, text, name)

  def hello(greeting: String, mid: String, name: String): Unit = {
    println(s"$greeting $mid ${name.reverse}!")
  }
}
