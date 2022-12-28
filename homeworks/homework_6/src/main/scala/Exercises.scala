object Exercises {


  def reverse[T](seq: Seq[T]): Seq[T] = seq.reverse

  /**
   * https://ru.wikipedia.org/wiki/Числа_Фибоначчи
   *
   * @param idx
   * @return
   */
  def fibonacci4Index(idx: Int): Int = {
    if (idx == 0) return 0
    if (idx < 3) return 1
    fibonacci4Index(idx - 1) + fibonacci4Index(idx - 2)
  }

  def fibonacci(idx: Int): Seq[Int] = {
    if (idx == 0) return Seq(0)
    var res = Seq(0, 1)
    for (i <- 2 to idx) {
      res = res :+ fibonacci4Index(i)
    }
    res
  }

  lazy val MORSE = Map("A" -> ".-", "B" -> "-...", "C" -> "-.-.", "D" -> "-..", "E" -> ".", "F" -> "..-.",
                       "G" -> "--.", "H" -> "....", "I" -> "..", "J" -> ".---", "K" -> "-.-", "L" -> ".-..",
                       "M" -> "--", "N" -> "-.", "O" -> "---", "P" -> ".--.", "Q" -> "--.-", "R" -> ".-.",
                       "S" -> "...", "T" -> "-", "U" -> "..-", "V" -> "...-", "W" -> ".--", "X" -> "-..-",
                       "Y" -> "-.--", "Z" -> "--..")

  def morse(text: String): String = {
    var res = ""
    for (i <- text.toUpperCase().map(sym => MORSE.getOrElse(sym.toString, sym))) {
      if ((i.isInstanceOf[String] || i.asInstanceOf[Char].isSpaceChar) && res != "")
        res = res + " " + i
      else
        res = res + i
      }
    res
  }


  def wordReverse(text: String): String = {
    val splitText = text.split(" ")
    var res = ""
    for (word <- splitText) {
      var toReverse = ""
      var ending = ""
      for (symbol <- word)
        if (symbol.isLetter) toReverse += symbol.toLower
        else ending += symbol
      var reversed = toReverse.reverse
      if (word(0).isUpper) reversed = reversed(0).toUpper + reversed.substring(1)
      for (i <- word.indices)
        if (word(i).isLetter) res += reversed(i)
      res += ending + " "
    }
    res.trim
  }
}
