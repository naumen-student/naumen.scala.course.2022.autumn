package example

object Main {
  def main(args: Array[String]): Unit = {
    val greeting = " Scala! This is "
    val name = "Anzhelika"
    val greetingWithName = greeting ++ name

    println("Hello" + greetingWithName)

    val greetingWords = Seq("Salva", "Ciao", "Buongiorno")

    greetingWords.foreach(arg => println(arg ++ greetingWithName))
    greetingWords.foreach(arg => println(arg ++ greeting ++ name.reverse))
  }
}