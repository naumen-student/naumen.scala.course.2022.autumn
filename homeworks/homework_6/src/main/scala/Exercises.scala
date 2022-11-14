import scala.collection.mutable

object Exercises {


  def reverse[T](seq: Seq[T]): Seq[T] = {
    var result: List[T] = List()
    for (i <- seq.indices) result = seq(i) :: result
    result
  }

  /**
   * https://ru.wikipedia.org/wiki/Числа_Фибоначчи
   *
   * @param idx
   * @return
   */
  def fibonacci4Index(idx: Int): Int = {
    val fib = fibonacci(idx)
    fib.last
  }

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

  def morse(text: String): String = {
    var answer = List[String]()
    for (i <- text.indices) {
      if (MORSE contains text(i).toUpper.toString) answer = answer :+ MORSE(text(i).toUpper.toString)
      else answer = answer :+ text(i).toString
    }
    answer.mkString(" ")
  }


  def wordReverse(text: String): String = {
    val words = text.split("[^\\p{L}*]").map { x => x.reverse}
      .filter(_.nonEmpty)
    val words1: mutable.MutableList[String] = mutable.MutableList()
    for (word <- words) {
      if (word.last.isUpper) {
        val strt = word.head
        words1.+=:(strt.toUpper.toString.concat(word.substring(1, word.length - 1)).concat(word.last.toLower.toString))
      } else words1.+=:(word)
    }
    val other = text.split("[\\p{L}*]").filter(_.nonEmpty)
    if (other.nonEmpty && text.startsWith(other(0))) {
      other.zipAll(words1, "", "").map(x => x._1 + x._2).mkString("")
    } else {
      words1.reverse.zipAll(other, "", "").map(x => x._1 + x._2).mkString("")
    }
  }

}