object Exercises {


  def reverse[T](seq: Seq[T]): Seq[T] = seq.reverse

  /**
   * https://ru.wikipedia.org/wiki/Числа_Фибоначчи
   *
   * @param idx
   * @return
   */
  def fibonacci4Index(idx: Int): Int = if (idx == 0) 0 else if (idx == 1 || idx == 2) 1 else
    fibonacci4Index(idx - 1) + fibonacci4Index(idx - 2)

  def fibonacci(idx: Int): Seq[Int] = {
    if (idx == 0) Seq(0)
    var seq = Seq(0, 1)
    for (_ <- 2 to idx) seq = seq :+ seq.drop(seq.length - 2).sum
    seq
  }

  lazy val MORSE = Map("A" -> ".-", "B" -> "-...", "C" -> "-.-.", "D" -> "-..", "E" -> ".", "F" -> "..-.",
                       "G" -> "--.", "H" -> "....", "I" -> "..", "J" -> ".---", "K" -> "-.-", "L" -> ".-..",
                       "M" -> "--", "N" -> "-.", "O" -> "---", "P" -> ".--.", "Q" -> "--.-", "R" -> ".-.",
                       "S" -> "...", "T" -> "-", "U" -> "..-", "V" -> "...-", "W" -> ".--", "X" -> "-..-",
                       "Y" -> "-.--", "Z" -> "--..")

  def morse(text: String): String = text
    .map(letter => if (MORSE contains letter.toUpper.toString) MORSE(letter.toUpper.toString) else letter)
    .mkString(" ")


  def wordReverse(text: String): String = text
    .split(" ")
    .map(string => string.replaceAll("[а-яёА-ЯЁ]+", string.replaceAll("[^а-яёА-ЯЁ]+", "").reverse))
    .map(string => {
      if (string.replaceAll("[^а-яёА-ЯЁ]+", "").last.isUpper)
        string.head.toUpper + string.tail.toLowerCase()
      else
        string
    })
    .mkString(" ")
}
