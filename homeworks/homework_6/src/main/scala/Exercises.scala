

object Exercises {


  def reverse[T](seq: Seq[T]): Seq[T] = {
    var result: List[T] = List()
    for (i <- seq.indices)
      result = seq(i) :: result
    result
  }

  /**
   * https://ru.wikipedia.org/wiki/Числа_Фибоначчи
   *
   * @param idx
   * @return
   */
  def fibonacci4Index(idx: Int): Int = {
    val fib = fibonacci(idx)
    fib.last
  }

  def fibonacci(idx: Int): Seq[Int] = {
    if (idx == 0) return Seq(0)
    var fib = Seq(0, 1)
    for (i <- 2 to idx)
      fib = fib :+ fib.drop(fib.length - 2).sum
    fib
  }

  lazy val MORSE = Map("A" -> ".-", "B" -> "-...", "C" -> "-.-.", "D" -> "-..", "E" -> ".", "F" -> "..-.",
                       "G" -> "--.", "H" -> "....", "I" -> "..", "J" -> ".---", "K" -> "-.-", "L" -> ".-..",
                       "M" -> "--", "N" -> "-.", "O" -> "---", "P" -> ".--.", "Q" -> "--.-", "R" -> ".-.",
                       "S" -> "...", "T" -> "-", "U" -> "..-", "V" -> "...-", "W" -> ".--", "X" -> "-..-",
                       "Y" -> "-.--", "Z" -> "--..")

  def morse(text: String): String =
    text.map(c => MORSE.getOrElse(c.toUpper.toString, c)).mkString(" ")


  def wordReverse(text: String): String = {
    val tokens = text.split(" ")
    val res = new StringBuilder()
    for (word <- tokens) {
      val toReverse = new StringBuilder()
      var ending = new StringBuilder()
      for (symbol <- word)
        if (symbol.isLetter)
          toReverse.append(symbol.toLower)
        else
          ending.append(symbol)
      var reversed = toReverse.reverse.toString

      if (word(0).isUpper)
        reversed = reversed(0).toUpper + reversed.substring(1)

      for (i <- word.indices)
        if (word(i).isLetter)
          res.append(reversed(i))
      res.append(ending).append(" ")
    }
    res.toString.trim
  }

}
