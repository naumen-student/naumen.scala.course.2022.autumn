
@main
def main(): Unit =
  val name = "Ololeg"
  val greetings = "Hello" :: "Priviet" :: "Dratvuti" :: Nil

  greetings.foreach(g => println(s"$g Scala! This is $name"))

  greetings.foreach(g => println(s"$g Scala! This is ${name.reverse}"))
