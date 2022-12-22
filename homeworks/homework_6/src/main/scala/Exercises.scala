import scala.collection.mutable.ListBuffer

object Exercises {


  def reverse[T](seq: Seq[T]): Seq[T] = seq.reverse


  def fibonacci4Index(idx: Int): Int = if (idx < 3) if (idx == 0) 0 else 1 else
    fibonacci4Index(idx - 1) + fibonacci4Index(idx - 2)

  def fibonacci(idx: Int): Seq[Int] = {
    var numbers = ListBuffer(0, 1, 1)
    if (idx + 1 <= numbers.length)
      return numbers.dropRight(idx + 1 - numbers.length)
    while (numbers.length < idx + 1)
      numbers += (numbers(numbers.length - 2) + numbers.last)
    numbers

  }

  lazy val MORSE = Map("A" -> ".-", "B" -> "-...", "C" -> "-.-.", "D" -> "-..", "E" -> ".", "F" -> "..-.",
    "G" -> "--.", "H" -> "....", "I" -> "..", "J" -> ".---", "K" -> "-.-", "L" -> ".-..",
    "M" -> "--", "N" -> "-.", "O" -> "---", "P" -> ".--.", "Q" -> "--.-", "R" -> ".-.",
    "S" -> "...", "T" -> "-", "U" -> "..-", "V" -> "...-", "W" -> ".--", "X" -> "-..-",
    "Y" -> "-.--", "Z" -> "--..")

  def morse(text: String): String = {
    val res = StringBuilder.newBuilder
    for (elem <- text.toUpperCase.toArray.map(x => MORSE.getOrElse(x.toString, x))) {
      if (res.nonEmpty && (elem.isInstanceOf[String] || elem.asInstanceOf[Char].isSpaceChar))
        res.append(" " + elem)
      else
        res.append(elem)
    }
    res.toString()
  }

  def wordReverse(text: String): String = {
    var res = StringBuilder.newBuilder
    for (e <- "(?U)(\\w+)|(\\W+)".r.findAllMatchIn(text).map(_.matched)) {
      var elem = e
      if (elem(0).isLetter) {
        elem = elem.reverse
        if (elem.last.isUpper)
          elem = elem.toLowerCase().capitalize
      }
      res.append(elem)

    }
    res.toString()
  }

}
