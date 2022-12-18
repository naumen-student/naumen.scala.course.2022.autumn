object Exercises {


  def reverse[T](seq: Seq[T]): Seq[T] = seq match {
    case (head: T) :: tail => reverse(tail) :+ head
    case Nil => Seq.empty
  }

  /**
   * https://ru.wikipedia.org/wiki/Числа_Фибоначчи
   *
   * @param idx
   * @return
   */
  def fibonacci4Index(idx: Int): Int = {
    idx match {
      case 1 | 2 => 1
      case _ => fibonacci4Index(idx - 1) + fibonacci4Index(idx - 2)
    }
  }

  def fibonacci(idx: Int): Seq[Int] = Range(0, idx + 1).foldLeft(Seq.empty[Int]){(acc, curr) => acc :+ (curr match {
      case 0 => 0
      case 1 | 2 => 1
      case index => acc(index - 1) + acc(index - 2)
    })
  }

  lazy val MORSE = Map("A" -> ".-", "B" -> "-...", "C" -> "-.-.", "D" -> "-..", "E" -> ".", "F" -> "..-.",
                       "G" -> "--.", "H" -> "....", "I" -> "..", "J" -> ".---", "K" -> "-.-", "L" -> ".-..",
                       "M" -> "--", "N" -> "-.", "O" -> "---", "P" -> ".--.", "Q" -> "--.-", "R" -> ".-.",
                       "S" -> "...", "T" -> "-", "U" -> "..-", "V" -> "...-", "W" -> ".--", "X" -> "-..-",
                       "Y" -> "-.--", "Z" -> "--..")

  def morse(text: String): String = text.split("").map{letter =>
    if (MORSE.contains(letter.toUpperCase)) MORSE(letter.toUpperCase) else letter
  }.mkString(" ")


  def wordReverse(text: String): String = {
    var reversedText = text
    getReversedMap(text.split("[^a-zA-Zа-яА-я]")).foreach{word =>
      reversedText = reversedText.replaceFirst(word._1, word._2)
    }
    reversedText
  }

  private def getReversedMap(words: Array[String]) = {
    words.filter(_.nonEmpty).map{ word =>
      val reversedWord = word.reverse.toLowerCase.toCharArray
      if (word(0).isUpper) reversedWord(0) = reversedWord(0).toUpper
      (word, reversedWord.mkString)
    }.toMap
  }
}
