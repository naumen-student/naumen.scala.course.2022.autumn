object Main extends App {
  def greeting(greet: String, name: String): Unit = println(s"$greet Scala! This is $name")
  val greets = List("Hello", "Hola", "Guten tag")
  val name = "Daniel Evdokimov"
  greets.foreach(greeting(_, name))
  greets.foreach(greeting(_, name.foldLeft(""){case (next, acc) => acc +: next}))
}
