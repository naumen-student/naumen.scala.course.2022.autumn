import scala.annotation.tailrec

object Exercises {


  def reverse[T](seq: Seq[T]): Seq[T] = if (seq.isEmpty) Nil else reverse(seq.tail) :+ seq.head

  /**
   * https://ru.wikipedia.org/wiki/Числа_Фибоначчи
   *
   * @param idx
   * @return
   */
  def fibonacci4Index(idx: Int): Int = if (idx == 1 || idx == 2) 1 else fibonacci4Index(idx - 1) + fibonacci4Index(idx - 2)

  def fibonacci(idx: Int): Seq[Int] = (2 to idx).foldLeft(Seq(0, 1)){
    (acc, i) => acc :+ acc(i - 1) + acc(i - 2)
  }

  lazy val MORSE = Map("A" -> ".-", "B" -> "-...", "C" -> "-.-.", "D" -> "-..", "E" -> ".", "F" -> "..-.",
                       "G" -> "--.", "H" -> "....", "I" -> "..", "J" -> ".---", "K" -> "-.-", "L" -> ".-..",
                       "M" -> "--", "N" -> "-.", "O" -> "---", "P" -> ".--.", "Q" -> "--.-", "R" -> ".-.",
                       "S" -> "...", "T" -> "-", "U" -> "..-", "V" -> "...-", "W" -> ".--", "X" -> "-..-",
                       "Y" -> "-.--", "Z" -> "--..")

  def morse(text: String): String = text
    .toUpperCase()
    .split("")
    .map(ltr => MORSE.getOrElse(ltr, ltr))
    .foldLeft(" ") {
      (resStr, currLtr) => if (currLtr.last != '.' && currLtr.last != '-' && currLtr != " ")
        resStr + currLtr
      else
        resStr + " " + currLtr
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
