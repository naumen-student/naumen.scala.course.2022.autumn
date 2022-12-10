import scala.annotation.tailrec

object Exercises {


  def reverse[T](seq: Seq[T]): Seq[T] = seq.foldLeft(Seq.empty[T]) {
    case (acc, e) => acc.prepended(e)
  }

  /**
   * https://ru.wikipedia.org/wiki/Числа_Фибоначчи
   *
   * @param idx
   * @return
   */
  def fibonacci4Index(idx: Int): Int = {
    @tailrec
    def go(i: Int, l1: Int, l2: Int): Int = i match {
      case i if i < 1 => l1
      case n => go(n - 1, l2, l1 + l2)
    }

    go(idx, 0, 1)
  }

  def fibonacci(idx: Int): Seq[Int] = (0 to idx).map(fibonacci4Index)

  lazy val MORSE = Map("A" -> ".-", "B" -> "-...", "C" -> "-.-.", "D" -> "-..", "E" -> ".", "F" -> "..-.",
    "G" -> "--.", "H" -> "....", "I" -> "..", "J" -> ".---", "K" -> "-.-", "L" -> ".-..",
    "M" -> "--", "N" -> "-.", "O" -> "---", "P" -> ".--.", "Q" -> "--.-", "R" -> ".-.",
    "S" -> "...", "T" -> "-", "U" -> "..-", "V" -> "...-", "W" -> ".--", "X" -> "-..-",
    "Y" -> "-.--", "Z" -> "--..")

  def morse(text: String): String = text.map(char => MORSE.getOrElse(char.toUpper.toString, char)).mkString(" ")


  def wordReverse(text: String): String = text.foldLeft(("", "")) {
    case ((text, word), e) if e.isLetter => (text, word + e)
    case ((text, word), e) => (text + word.headOption.fold("")(h => if (h.isUpper) word.toLowerCase.reverse.capitalize else word.reverse) + e, "")
  } match {
    case (text, word) => text + word
  }

}
