package example

object Main extends App{

  val greetings = Array("Hello", "Hola", "Guten tag")
  val str = "Scala! This is"
  val name = "Alex"

  def printGretings(name: String) = greetings.foreach(greeting => println(s"$greeting $str $name"))

  printGretings(name)
  printGretings(name.reverse)

}
