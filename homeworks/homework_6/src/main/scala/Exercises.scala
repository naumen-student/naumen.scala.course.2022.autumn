import scala.annotation.tailrec

object Exercises {


  def reverse[T](seq: Seq[T]): Seq[T] = seq.foldLeft(Seq.empty[T])((acc, x) => x +: acc)

  /**
   * https://ru.wikipedia.org/wiki/Числа_Фибоначчи
   *
   * @param idx
   * @return
   */

  def fibonacci4Index(idx: Int): Int = {
    @tailrec
    def fib(idx: Int, first: Int = 0, second: Int = 1): Int =
      if (idx == 0)
        first
      else if (idx == 1)
        second
      else
        fib(idx - 1, second, first + second)

    fib(idx)
  }

  def fibonacci(idx: Int): Seq[Int] =
    (0 to idx).map(fibonacci4Index)


  lazy val MORSE = Map("A" -> ".-", "B" -> "-...", "C" -> "-.-.", "D" -> "-..", "E" -> ".", "F" -> "..-.",
    "G" -> "--.", "H" -> "....", "I" -> "..", "J" -> ".---", "K" -> "-.-", "L" -> ".-..",
    "M" -> "--", "N" -> "-.", "O" -> "---", "P" -> ".--.", "Q" -> "--.-", "R" -> ".-.",
    "S" -> "...", "T" -> "-", "U" -> "..-", "V" -> "...-", "W" -> ".--", "X" -> "-..-",
    "Y" -> "-.--", "Z" -> "--..")

  def morse(text: String): String = text.toUpperCase.map(s => MORSE.getOrElse(s.toString, s.toString)).mkString(" ")


  def wordReverse(text: String): String = text.foldLeft(("", "")) {
    case ((text, word), e) if e.isLetter => (text, word + e)
    case ((text, word), e) => (text + word.headOption.fold("")(h => if (h.isUpper) word.toLowerCase.reverse.capitalize else word.reverse) + e, "")
  } match {
    case (text, word) => text + word
  }
}
