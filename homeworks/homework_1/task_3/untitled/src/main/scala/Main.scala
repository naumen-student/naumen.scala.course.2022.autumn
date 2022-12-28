object Main extends App {
  def printHello(prelude: String, name: String): Unit =
    println(s"$prelude$name")

  val name = "tfox"

  val helloText = "Hello Scala! This is "

  printHello(helloText, name)
  val holaText = helloText.replace("Hello", "Hola")

  printHello(holaText, name)

  val reverseName = name.reverse

  printHello(helloText, reverseName)

  printHello(holaText, reverseName)

}
