package example

object Main extends App{
  def greeting(greet: String, name: String): String= s"${greet}, scala! This is ${name}"
  var name = "Artyom"
  var reversedName = name.reverse

  var englishGreeting = "Hello"
  var spanishGreeting = "Hola"
  var germanGreeting = "Guten tag"

  var greetings = Array(englishGreeting, spanishGreeting, germanGreeting)

  for (greet <- greetings) {
    println(greeting(greet, name))
  }

  for (greet <- greetings) {
    println(greeting(greet, reversedName))
  }
}
