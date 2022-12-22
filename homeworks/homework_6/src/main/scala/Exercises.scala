import scala.collection.mutable

object Exercises {
  def reverse[T](seq: Seq[T]): Seq[T] = {
    var res: List[T] = List()
    for (i <- seq.indices) res = seq(i) :: res
    res
  }

  /**
   * https://ru.wikipedia.org/wiki/Числа_Фибоначчи
   *
   * @param idx
   * @return
   */
  def fibonacci4Index(idx: Int): Int =
    if (idx < 3)
      if (idx == 0) 0
      else 1
    else fibonacci4Index(idx - 1) + fibonacci4Index(idx - 2)

  def fibonacci(idx: Int): Seq[Int] = {
    if (idx == 0) return Seq(0)
    var fib = Seq(0, 1)
    for (_ <- 2 to idx) fib = fib :+ fib.drop(fib.length - 2).sum
    fib
  }

  lazy val MORSE: Map[String, String] = Map("A" -> ".-", "B" -> "-...", "C" -> "-.-.", "D" -> "-..", "E" -> ".", "F" -> "..-.",
    "G" -> "--.", "H" -> "....", "I" -> "..", "J" -> ".---", "K" -> "-.-", "L" -> ".-..",
    "M" -> "--", "N" -> "-.", "O" -> "---", "P" -> ".--.", "Q" -> "--.-", "R" -> ".-.",
    "S" -> "...", "T" -> "-", "U" -> "..-", "V" -> "...-", "W" -> ".--", "X" -> "-..-",
    "Y" -> "-.--", "Z" -> "--..")

  def morse(text: String): String = {
    text
      .map(symbol => if (MORSE contains symbol.toUpper.toString) MORSE(symbol.toUpper.toString) else symbol)
      .mkString(" ")
  }

  def wordReverse(text: String): String = {
    val splitedText = text.split(" ")
    val res = new mutable.StringBuilder
    for (str <- splitedText) {
      val toReverse = new mutable.StringBuilder
      for (symb <- str)
        if (symb.isLetter) toReverse += symb.toLower

      val reversed = toReverse.reverse
      if (str(0).isUpper) reversed(0) = reversed(0).toUpper

      for (i <- str.indices)
        if (str(i).isLetter) res += reversed(i)
        else res += str(i)

      res += ' '
    }
    res.init.toString()
  }
}
