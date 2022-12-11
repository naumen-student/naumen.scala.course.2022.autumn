object Exercises {


  def reverse[T](seq: Seq[T]): Seq[T] =
    seq.toList.foldLeft(List.empty[T])((acc, x) => x :: acc)

  /**
   * https://ru.wikipedia.org/wiki/Числа_Фибоначчи
   *
   * @param idx
   * @return
   */
  def fibonacci4Index(idx: Int): Int =
    ((Math.pow((1 + Math.sqrt(5)) / 2, idx) - Math.pow((1 - Math.sqrt(5)) / 2, idx)) / Math.sqrt(5)).toInt

  def fibonacci(idx: Int): Seq[Int] =
    (0 to idx).map(fibonacci4Index)

  lazy val MORSE = Map("A" -> ".-", "B" -> "-...", "C" -> "-.-.", "D" -> "-..", "E" -> ".", "F" -> "..-.",
    "G" -> "--.", "H" -> "....", "I" -> "..", "J" -> ".---", "K" -> "-.-", "L" -> ".-..",
    "M" -> "--", "N" -> "-.", "O" -> "---", "P" -> ".--.", "Q" -> "--.-", "R" -> ".-.",
    "S" -> "...", "T" -> "-", "U" -> "..-", "V" -> "...-", "W" -> ".--", "X" -> "-..-",
    "Y" -> "-.--", "Z" -> "--..")

  def morse(text: String): String =
    text.toUpperCase.map(c => MORSE.getOrElse(c.toString, c.toString)).mkString(" ")

  def wordReverse(text: String): String = text.foldLeft(("", "")) {
    case ((text, word), e) if e.isLetter => (text, word + e)
    case ((text, word), e) => (text + (if (word.headOption.forall(_.isUpper)) word.reverse.toLowerCase.capitalize else word.reverse) + e, "")
  } match {
    case (text, word) => text + word
  }

}
