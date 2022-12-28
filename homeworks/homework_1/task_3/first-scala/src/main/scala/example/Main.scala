package example

object Main extends App{
  def greetingScala(greeting: String, name: String ): Unit = {
    println(s"$greeting Scala. This is $name")
  }
  val greetings = List("Hello", "Guten tag", "Hola")
  var name = "Il'dar Yumagulov"
  greetings.foreach(greeting => greetingScala(greeting, name))
  greetings.foreach(greeting => greetingScala(greeting, name.reverse))
}
