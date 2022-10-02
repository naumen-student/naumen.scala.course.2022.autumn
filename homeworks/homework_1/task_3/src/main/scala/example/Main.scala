package example

object Main extends App{
  val greeting = (helloVersion: String, name: String) => println(s"$helloVersion Scala!This is $name")
  val greetingVersions: List[String] = List("Hello", "Hola", "Guten Tag")
  val name: String = "Grisha"
  for (version <- greetingVersions) greeting(version, name)
  for (version <- greetingVersions) greeting(version, name.reverse)
}
