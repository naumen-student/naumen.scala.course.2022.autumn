import scala.::
import scala.collection.mutable.ListBuffer


object Exercises {

  def reverse[T](seq: Seq[T]): Seq[T] = {
    def reverseInner[T](result: Seq[T], list: Seq[T]) : Seq[T] = {
      list match {
        case Nil => result
        case el :: els => {reverseInner(el :: result, els)}
      }
    }

    reverseInner(Nil, seq);
  }

  /**
   * https://ru.wikipedia.org/wiki/Числа_Фибоначчи
   *
   * @param idx
   * @return
   */
  def fibonacci4Index(idx: Int): Int = getFib(idx)

  def fibonacci(idx: Int): Seq[Int] = {
    for {i <- 0 to idx} yield getFib(i)
  }

  def getFib(idx: Int) : Int = {
    if (idx == 0) 0
    else if (idx == 1) 1
    else getFib(idx - 2) + getFib(idx - 1)
  }


  lazy val MORSE = Map("A" -> ".-", "B" -> "-...", "C" -> "-.-.", "D" -> "-..", "E" -> ".", "F" -> "..-.",
    "G" -> "--.", "H" -> "....", "I" -> "..", "J" -> ".---", "K" -> "-.-", "L" -> ".-..",
    "M" -> "--", "N" -> "-.", "O" -> "---", "P" -> ".--.", "Q" -> "--.-", "R" -> ".-.",
    "S" -> "...", "T" -> "-", "U" -> "..-", "V" -> "...-", "W" -> ".--", "X" -> "-..-",
    "Y" -> "-.--", "Z" -> "--..")

  def morse(text: String): String = {
    def getMorseSymbol(symbol: String):String = {
      val upper = symbol.toUpperCase
      if (MORSE.contains(upper))
        MORSE(upper)
      else symbol
    }

    text.map(s => getMorseSymbol(s.toString)).mkString(" ")

  }


  def wordReverse(text: String): String = {
    def getCapitals(text : String): Seq[Int] = {
      for {i <- 0 until text.length if text(i).isUpper } yield i
    }

    val punctuationMarks = text.split("[a-zA-ZА-Яа-я]+").reverse.init.reverse
    val words = {
      text.split("[ !.?,]").filter(p => p != "")
    }

    words.map({
      case word if word.head.isUpper => word.toLowerCase.reverse.capitalize
      case word => word.reverse
    }).zip(punctuationMarks).map({case(str, s) => s"$str$s"}).mkString("")
  }

}