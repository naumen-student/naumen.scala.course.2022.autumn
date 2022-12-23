object Exercises {

  def reverse[T](seq: Seq[T]): Seq[T] = seq.reverse

  /** https://ru.wikipedia.org/wiki/Числа_Фибоначчи
    *
    * @param idx
    * @return
    */
  def fibonacci4Index(idx: Int): Int = fibonacci(idx).last

  def fibonacci(idx: Int): Seq[Int] = Iterator.iterate((0, 1)) { case (prev, curr) => (curr, prev + curr) }.map(_._1)
    .take(idx + 1).toSeq

  lazy val MORSE: Map[String, String] = Map(
    "A" -> ".-",
    "B" -> "-...",
    "C" -> "-.-.",
    "D" -> "-..",
    "E" -> ".",
    "F" -> "..-.",
    "G" -> "--.",
    "H" -> "....",
    "I" -> "..",
    "J" -> ".---",
    "K" -> "-.-",
    "L" -> ".-..",
    "M" -> "--",
    "N" -> "-.",
    "O" -> "---",
    "P" -> ".--.",
    "Q" -> "--.-",
    "R" -> ".-.",
    "S" -> "...",
    "T" -> "-",
    "U" -> "..-",
    "V" -> "...-",
    "W" -> ".--",
    "X" -> "-..-",
    "Y" -> "-.--",
    "Z" -> "--.."
  )

  def morse(text: String): String = text.replaceAll(" ", "   ").replaceAll("([a-zA-Z])(?=[a-zA-Z])", "$1 ")
    .map(x => MORSE.getOrElse(x.toUpper.toString, x)).mkString

  def wordReverse(text: String): String = {
    val replaceCase: String => String = _.toList match {
      case Nil => ""
      case head :: tl if head.isUpper && tl.length > 1 =>
        head.toLower + tl.take(tl.length - 1).mkString + tl.last.toUpper
      case list => list.mkString
    }
    val wordsWithSymbols = text.split(' ').toList
    val regex = """([^a-zA-ZА-Яа-я]*)([a-zA-ZА-Яа-я]*)([^a-zA-ZА-Яа-я]*)""".r("snw", "w", "enw")
    (for {
      wordWithSymbols <- wordsWithSymbols
      word = wordWithSymbols
      matches = regex.findAllMatchIn(word)
      lul = matches.map(m => s"${m.group("snw")}${replaceCase(m.group("w")).reverse}${m.group("enw")}")
    } yield lul.mkString).mkString(" ")
  }
}
