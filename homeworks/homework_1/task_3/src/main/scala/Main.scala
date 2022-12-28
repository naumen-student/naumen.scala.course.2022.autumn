package kek.lol.arbidol

object Main extends App {
  val name = "Alexander Paschenko"

  List("Hello", "Hola", "Guten Tag").map(privet =>
    println(s"${privet} Scala! This is ${name}")
  )

  List("Hello", "Hola", "Guten Tag").map(privet =>
    println(s"${privet} Scala! This is ${name.reverse}")
  )
}
