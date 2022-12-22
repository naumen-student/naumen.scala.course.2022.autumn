import scala.collection.mutable

object Exercises {


  def reverse[T](seq: Seq[T]): Seq[T] = {
    var res: List[T] = List()
    for (i <- seq.indices)
      res = seq(i) :: res
    res
  }

  /**
   * https://ru.wikipedia.org/wiki/Числа_Фибоначчи
   *
   * @param idx
   * @return
   */
  def fibonacci4Index(idx: Int): Int = {
    var a: Int = 0
    var b: Int = 1
    for (i <- 0 until idx - 1) {
      val temp = b
      b = a + b
      a = temp
    }
    b
  }

  def fibonacci(idx: Int): Seq[Int] = {
    if (idx == 0) return Seq(0)
    var a: Int = 0
    var b: Int = 1
    var ans = Seq(fib1, fib2)
    for (i <- 0 until idx - 1) {
      val temp = b
      b = a + b
      ans = ans :+ b
      a = temp
    }
    ans
  }

  lazy val MORSE = Map("A" -> ".-", "B" -> "-...", "C" -> "-.-.", "D" -> "-..", "E" -> ".", "F" -> "..-.",
                       "G" -> "--.", "H" -> "....", "I" -> "..", "J" -> ".---", "K" -> "-.-", "L" -> ".-..",
                       "M" -> "--", "N" -> "-.", "O" -> "---", "P" -> ".--.", "Q" -> "--.-", "R" -> ".-.",
                       "S" -> "...", "T" -> "-", "U" -> "..-", "V" -> "...-", "W" -> ".--", "X" -> "-..-",
                       "Y" -> "-.--", "Z" -> "--..")

  def morse(text: String): String = text.map(symb => if (MORSE contains symb.toUpper.toString) MORSE(symb.toUpper.toString) else symb).mkString(" ")


  def wordReverse(text: String): String = {
    val splitText = text.split(" ")
    val ans: mutable.StringBuilder = new mutable.StringBuilder
    for (line <- splitText) {
      val toRev: mutable.StringBuilder = new mutable.StringBuilder
      for (symb <- line) {
        if (symb.isLetter) {
          toRev += symb.toLower
        }
      }
      val rev = toRev.reverse
      if (line(0).isUpper) {
        rev(0) = rev(0).toUpper
      }
      for (i <- line.indices) {
        if (line(i).isLetter) {
          ans += rev(i)
        } else {
          ans += line(i)
        }
      }
      ans += ' '
    }
    ans.init.toString()
  }
}
