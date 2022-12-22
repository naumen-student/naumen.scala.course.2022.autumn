object Main {
  def main(args: Array[String]): Unit = {
    def greeting(name: String): String = s"Hello, scala! This is ${name}."

    val name: String = "Simon"
    println(greeting(name))

    def germanGreeting(name: String): String =
      greeting(name).replace("Hello", "Guten Abend")

    println(germanGreeting(name))

    val reversedName: String = name.reverse

    println(greeting(reversedName))
    println(germanGreeting(reversedName))

  }
}