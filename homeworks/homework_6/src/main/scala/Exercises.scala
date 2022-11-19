import scala.annotation.tailrec

object Exercises {


  def reverse[T](seq: Seq[T]): Seq[T] = seq.foldRight(Seq.empty[T]){
    case (acc, next) => next :+ acc
  }

  /**
   * https://ru.wikipedia.org/wiki/Числа_Фибоначчи
   *
   * @param idx
   * @return
   */
  def fibonacci4Index(idx: Int): Int = fib_go(idx)(idx)

  def fibonacci(idx: Int): Seq[Int] = fib_go(idx)

  @tailrec
  def fib_go(idx: Int, acc: Seq[Int] = Seq(0, 1)): Seq[Int] =
    if (acc.length - 1 >= idx){
      acc
    } else {
      fib_go(idx, acc :+ acc(acc.length - 2) + acc.last)
    }

  lazy val MORSE = Map("A" -> ".-", "B" -> "-...", "C" -> "-.-.", "D" -> "-..", "E" -> ".", "F" -> "..-.",
                       "G" -> "--.", "H" -> "....", "I" -> "..", "J" -> ".---", "K" -> "-.-", "L" -> ".-..",
                       "M" -> "--", "N" -> "-.", "O" -> "---", "P" -> ".--.", "Q" -> "--.-", "R" -> ".-.",
                       "S" -> "...", "T" -> "-", "U" -> "..-", "V" -> "...-", "W" -> ".--", "X" -> "-..-",
                       "Y" -> "-.--", "Z" -> "--..")

  def morse(text: String): String = text.split("").map(ch => MORSE.getOrElse(ch.toUpperCase, ch)).mkString(" ")


  def wordReverse(text: String): String = {
    @tailrec
    def go(cur: Int = 0, startFromUpper: Boolean = false, word: List[String] = List.empty[String], res: List[String] = List.empty[String]): String ={
      if (res.length == text.length) {
        res.mkString
      } else {
        val char = text.charAt(cur)
        if(char.isLetter && char.isUpper){
          go(cur + 1, startFromUpper = true, char.toLower.toString +: word, res)
        } else if (char.isLetter) {
          go(cur + 1, startFromUpper, char.toString +: word, res)
        } else if (startFromUpper){
          go(cur + 1, res = ((res :+ word.head.toUpperCase) ++ word.tail) :+ char.toString)
        } else {
          go(cur + 1, res = (res ++ word) :+ char.toString)
        }
      }
    }

    go()
  }
}
