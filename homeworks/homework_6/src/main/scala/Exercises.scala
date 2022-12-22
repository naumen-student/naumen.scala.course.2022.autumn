import scala.annotation.tailrec

object Exercises extends App {

  def reverse[T](seq: Seq[T]): Seq[T] = seq.reverse

  /**
   * https://ru.wikipedia.org/wiki/Числа_Фибоначчи
   *
   * @param idx
   * @return
   */
  private def fibonacciLL = LazyList.iterate((0, 1)) {
    case (prev, next) => (next, prev + next)
  }

  def fibonacci4Index(idx: Int): Int =
    fibonacciLL.zipWithIndex.find(_._2 == idx).get._1._1

  def fibonacci(idx: Int): Seq[Int] = fibonacciLL.take(idx + 1).toList.map(_._1)

  lazy val MORSE = Map("A" -> ".-", "B" -> "-...", "C" -> "-.-.", "D" -> "-..", "E" -> ".", "F" -> "..-.",
                       "G" -> "--.", "H" -> "....", "I" -> "..", "J" -> ".---", "K" -> "-.-", "L" -> ".-..",
                       "M" -> "--", "N" -> "-.", "O" -> "---", "P" -> ".--.", "Q" -> "--.-", "R" -> ".-.",
                       "S" -> "...", "T" -> "-", "U" -> "..-", "V" -> "...-", "W" -> ".--", "X" -> "-..-",
                       "Y" -> "-.--", "Z" -> "--..")

  def morse(text: String): String = {
    @tailrec
    def wordReverseRec(text: List[Char], readQueue: Vector[String], result: Vector[String]): Vector[String] =
      text match {
        case head :: tail if head.isSpaceChar => wordReverseRec(tail, readQueue.appended(" "), result)
        case head :: tail => MORSE.get(head.toUpper.toString) match {
          case Some(symbols) => wordReverseRec(tail, readQueue.appended(symbols), result)
          case None => wordReverseRec(tail, Vector.empty, result.appended(readQueue.mkString(" ")).appended(head.toString))
        }

        case Nil => result.appended(readQueue.mkString(" "))
      }

    wordReverseRec(text.toList, Vector.empty, Vector.empty)
  }.mkString

  def wordReverse(text: String): String = {
    val charList = text.toList
    val headMask = charList.zipWithIndex.collect {
      case (c, i) if c.isLetter && c.isUpper => i
    }.toSet
    @tailrec
    def wordReverseRec(text: List[Char], readStack: List[Char], result: Vector[Char]): Vector[Char] =
      text match {
        case head :: tail if head.isLetter =>
          wordReverseRec(tail, head.toLower :: readStack, result)
        case head :: tail =>
          wordReverseRec(tail, Nil, result :++ readStack :+ head)

        case Nil => result
      }
    wordReverseRec(charList, Nil, Vector.empty)
      .zipWithIndex
      .map {
        case (c, i) => if(headMask.contains(i)) c.toUpper else c
      }.mkString
  }


}
