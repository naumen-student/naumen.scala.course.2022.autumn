package example


object Main extends App{
  def getFullGreeting(greeting: String = "Hello", name: String = "World"): String = {
    s"${greeting} Scala! This is ${name}"
  }

  def getForeignGreeting(lang: String = "eng"): String = {
    val defaultGreeting = "Hello"
    val foreignGreetings = Map("eng" -> "Hello", "esp" -> "Halo", "fr" -> "Salut", "de" -> "Guten Tag")
    if(foreignGreetings.contains(lang))
      foreignGreetings(lang)
    else
      defaultGreeting
  }

  def greeting(lang: String, name: String) = println(getFullGreeting(getForeignGreeting(lang), name))
  def greetingNameReverse(lang: String, name: String) = greeting(lang, name.reverse)

  greetingNameReverse("rus", "Sidney")
}
