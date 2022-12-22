object Main extends App {
  def reverse(s: String): String = {
    var res = ""
    for (i <- s.length() - 1 to 0 by -1) res = res + s(i)
    res
  }
  val myName = "Sergey"
  val allGreetings = Seq("Hello", "Hola", "Guten tag", "Bonjour", "Privet")
  println(s"Hello Scala! This is ${myName}")
  val getUserScalaGreetings = (greetings: String, userName: String) => s"${greetings} Scala! This is ${userName}"
  for (greeting <- allGreetings) println(getUserScalaGreetings(greeting, myName))
  for (greeting <- allGreetings) println(getUserScalaGreetings(greeting, myName.toUpperCase()))
  for (greeting <- allGreetings) println(getUserScalaGreetings(greeting, reverse(myName)))
}