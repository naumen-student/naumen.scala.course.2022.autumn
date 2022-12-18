object Exercises {


  def reverse[T](seq: Seq[T]): Seq[T] = seq.reverse

  /**
   * https://ru.wikipedia.org/wiki/Числа_Фибоначчи
   *
   * @param idx
   * @return
   */
  def fibonacci4Index(idx: Int): Int = if (idx < 3) if (idx == 0) 0 else 1 else
    fibonacci4Index(idx - 1) + fibonacci4Index(idx - 2)

  def fibonacci(idx: Int): Seq[Int] = {
    if (idx == 0) return Seq(0)
    var fib = Seq(0, 1)
    for (_ <- 2 to idx) fib = fib :+ fib.drop(fib.length - 2).sum
    fib
  }



  lazy val MORSE = Map("A" -> ".-", "B" -> "-...", "C" -> "-.-.", "D" -> "-..", "E" -> ".", "F" -> "..-.",
                       "G" -> "--.", "H" -> "....", "I" -> "..", "J" -> ".---", "K" -> "-.-", "L" -> ".-..",
                       "M" -> "--", "N" -> "-.", "O" -> "---", "P" -> ".--.", "Q" -> "--.-", "R" -> ".-.",
                       "S" -> "...", "T" -> "-", "U" -> "..-", "V" -> "...-", "W" -> ".--", "X" -> "-..-",
                       "Y" -> "-.--", "Z" -> "--..")

  def morse(text: String): String = text
    .toUpperCase()
    .split("")
    .map(sym => MORSE.getOrElse(sym, sym))
    .foldLeft(" ") {
      (rSymb, cSymb) => if (cSymb.last != '.' && cSymb.last != '-' && cSymb != " ")
        rSymb + cSymb
      else
        rSymb + " " + cSymb
    }
    .trim

  def wordReverse(text: String): String = text
    .split(" ")
    .map(str => str.replaceAll("[а-яёА-ЯЁ]+", str.replaceAll("[^а-яёА-ЯЁ]+", "").reverse))
    .map(str => {
      if (str.replaceAll("[^а-яёА-ЯЁ]+", "").last.isUpper)
        str.head.toUpper + str.tail.toLowerCase()
      else
        str
    })
    .mkString(" ")
}
