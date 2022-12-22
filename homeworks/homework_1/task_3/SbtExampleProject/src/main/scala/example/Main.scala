package example

object Main {
  def main(args: Array[String]): Unit =
    def hello(name: String): String = s"Hello, scala! This is ${name}"
    val name: String = "Arina"
    println(hello(name))

    def spanishHello(name: String): String =
      hello(name).replace("Hello", "Hola")

    println(spanishHello(name))

    val reversedName: String = name.reverse

    println(hello(reversedName))
    println(spanishHello(reversedName))

}
