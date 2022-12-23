package exmaple

object Main extends App {
  def sayHi(name: String, greeting: String, rev: Boolean) = {
    if (rev) {
      println(greeting + " Scala! This is " + name.reverse)
    } else {
      println(greeting + " Scala! This is " + name)
    }
  }

  sayHi("Igor", "Privet", true)
  sayHi("Igor", "Privet", false)
  sayHi("Igor", "Hi", true)
  sayHi("Igor", "Hi", false)
}
