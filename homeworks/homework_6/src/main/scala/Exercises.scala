import scala.annotation.tailrec

object Exercises {


  def reverse[T](seq: Seq[T]): Seq[T] = seq match {
    case element :: Nil => Seq(element)
    case element :: tail => reverse(tail) :+ element
  }

  /**
   * https://ru.wikipedia.org/wiki/Числа_Фибоначчи
   *
   * @param idx
   * @return
   */
  def fibonacci4Index(idx: Int): Int = fibonacciAccumulator(0, 1, idx)

  @tailrec
  def fibonacciAccumulator(a: Int, b: Int, i: Int): Int = i match {
    case 0 => a
    case i => fibonacciAccumulator(b, a + b, i - 1)
  }

  def fibonacci(idx: Int): Seq[Int] =
    (0 to idx).map(fibonacci4Index)

//  @tailrec
//  def fibonacciListAccumulator(list: List[Int], a: Int, b: Int, i: Int): List[Int] = i match {
//    case 0 => a :: Nil
//    case i => fibonacciListAccumulator()
//  }

  lazy val MORSE = Map("A" -> ".-", "B" -> "-...", "C" -> "-.-.", "D" -> "-..", "E" -> ".", "F" -> "..-.",
    "G" -> "--.", "H" -> "....", "I" -> "..", "J" -> ".---", "K" -> "-.-", "L" -> ".-..",
    "M" -> "--", "N" -> "-.", "O" -> "---", "P" -> ".--.", "Q" -> "--.-", "R" -> ".-.",
    "S" -> "...", "T" -> "-", "U" -> "..-", "V" -> "...-", "W" -> ".--", "X" -> "-..-",
    "Y" -> "-.--", "Z" -> "--..")

  def morse(text: String): String =
    text
      .toUpperCase
      .map(s => MORSE.getOrElse(s.toString, s.toString))
      .mkString(" ")


  def wordReverse(text: String): String = text.foldLeft(("", "")) {
    case ((text, word), char) if char.isLetter => (text, word + char)
    case ((text, word), char) => (text + word.headOption.fold("")(c => if (c.isUpper) word.toLowerCase.reverse.capitalize else word.reverse) + char, "")
  } match {
    case (text, word) => text + word
  }
}
