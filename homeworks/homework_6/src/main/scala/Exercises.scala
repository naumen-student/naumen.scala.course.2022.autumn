object Exercises {

  def reverse[T](seq: Seq[T]): Seq[T] = {
    var reversedSequence = scala.collection.mutable.Seq[T]()
    seq.foreach(elem => reversedSequence = elem +: reversedSequence)
    reversedSequence
  }

  /**
   * https://ru.wikipedia.org/wiki/Числа_Фибоначчи
   *
   * @param idx
   * @return
   */
  def fibonacci4Index(idx: Int): Int = {
    idx match {
      case 0 => 0
      case 1 => 1
      case _ => fibonacci4Index(idx - 1) + fibonacci4Index(idx - 2)
    }
  }

  def fibonacci(idx: Int): Seq[Int] = {
    Seq.range(0, idx + 1).map(index => fibonacci4Index(index))
  }

  lazy val MORSE = Map("A" -> ".-", "B" -> "-...", "C" -> "-.-.", "D" -> "-..", "E" -> ".", "F" -> "..-.",
    "G" -> "--.", "H" -> "....", "I" -> "..", "J" -> ".---", "K" -> "-.-", "L" -> ".-..",
    "M" -> "--", "N" -> "-.", "O" -> "---", "P" -> ".--.", "Q" -> "--.-", "R" -> ".-.",
    "S" -> "...", "T" -> "-", "U" -> "..-", "V" -> "...-", "W" -> ".--", "X" -> "-..-",
    "Y" -> "-.--", "Z" -> "--..")

  def morse(text: String): String = {
    var result = text.toCharArray.map(char=>{
      if (MORSE.contains(char.toUpper.toString))
        MORSE(char.toUpper.toString)
      else
        char.toString
    }).mkString(" ")
    result
  }


  def wordReverse(text: String): String = {
    var res = ""
    for (e <- "(?U)(\\w+)|(\\W+)".r.findAllMatchIn(text).map(_.matched)) {
      var elem = e
      if (elem(0).isLetter) {
        elem = elem.reverse
        if (elem.last.isUpper)
          elem = elem.toLowerCase().capitalize
      }
      res = res.concat(elem)

    }
    res

  }

}
