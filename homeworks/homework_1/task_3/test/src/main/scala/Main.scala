object Main extends App {
  val middle = " Scala! This is "
  val name = "Artem"
  val his = Seq("Hello", "Hi", "Privet", "Ola")

  his.foreach(arg => println(arg ++ middle ++ name))
  his.foreach(arg => println(arg ++ middle ++ name.reverse))
}
