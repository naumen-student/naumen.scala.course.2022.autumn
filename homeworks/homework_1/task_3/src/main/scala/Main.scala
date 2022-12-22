object Main {
  def main(args: Array[String]): Unit = {
    def hello(name: String): String = s"Hello, scala! This is ${name}."

    def germanHello(name: String): String = hello(name).replace("Hello", "Guten Abend")

    val name: String = "Egor"
    val rev: String = name.reverse

    println(hello(name))
    println(germanHello(name))
    println(hello(rev))
    println(germanHello(rev))
  }
}