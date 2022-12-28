object HelloScala {
  def main(args: Array[String]): Unit = {
    val name = "Daria Katyaeva"
    val hi = List("Hello", "Привет", "Hola", "Bonjour")
    val printHi = (h: List[String], n: String) => h.foreach(arg => println(s"$arg Scala! This is $n"))
    printHi(hi, name)
    printHi(hi, name.reverse)
  }
}
