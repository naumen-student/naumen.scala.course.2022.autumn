package example

object Main extends App {
  var welcome = "Hello"
  val staticPart = " Scala! This is "
  val name = "Ekaterina"
  
  println(welcome + staticPart + name)

  val variousGreetings = new Array[String](2)
  variousGreetings(0) = "Hola"
  variousGreetings(1) = "Guten tag"

  for (i <- 0 to 1) println(variousGreetings(i) + staticPart + name)

  for (i <- 0 to 1) println(variousGreetings(i) + staticPart + name.reverse)

}
