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
    var fib1: Int = 0
    var fib2: Int = 1

    for (_ <- 0 until  idx - 1) {
      val temp = fib2
      fib2 = fib1 + fib2
      fib1 = temp
    }
    fib2
  }

  def fibonacci(idx: Int): Seq[Int] = {
    if (idx == 0) return Seq(0)
    var fib1: Int = 0
    var fib2: Int = 1
    var res = Seq(fib1, fib2)
    for (_ <- 0 until  idx - 1) {
      val temp = fib2
      fib2 = fib1 + fib2
      res = res :+ fib2
      fib1 = temp
    }
    res
  }

  lazy val MORSE = Map("A" -> ".-", "B" -> "-...", "C" -> "-.-.", "D" -> "-..", "E" -> ".", "F" -> "..-.",
                       "G" -> "--.", "H" -> "....", "I" -> "..", "J" -> ".---", "K" -> "-.-", "L" -> ".-..",
                       "M" -> "--", "N" -> "-.", "O" -> "---", "P" -> ".--.", "Q" -> "--.-", "R" -> ".-.",
                       "S" -> "...", "T" -> "-", "U" -> "..-", "V" -> "...-", "W" -> ".--", "X" -> "-..-",
                       "Y" -> "-.--", "Z" -> "--..")

  def morse(text: String): String = {
    text.map(symb => if (MORSE contains symb.toUpper.toString) MORSE(symb.toUpper.toString) else symb).mkString(" ")
  }


  def wordReverse(text: String): String = {
    val splited = text.split(" ")
    val res: mutable.StringBuilder = new mutable.StringBuilder
    for (str <- splited) {
      val toReverse: mutable.StringBuilder = new mutable.StringBuilder
      for (symb <- str) {
        if (symb.isLetter) {
          toReverse += symb.toLower
        }
      }
      val reversed = toReverse.reverse
      if (str(0).isUpper) {
        reversed(0) = reversed(0).toUpper
      }
      for (i <- str.indices) {
        if (str(i).isLetter) {
          res += reversed(i)
        } else {
          res += str(i)
        }
      }
      res += ' '
    }
    res.init.toString()
  }
}