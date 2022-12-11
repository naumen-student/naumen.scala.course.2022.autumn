import scala.collection.mutable

object Exercises {


    def reverse[T](seq: Seq[T]): Seq[T] = if (seq.isEmpty) Nil else reverse(seq.tail) :+ seq.head

    /**
     * https://ru.wikipedia.org/wiki/Числа_Фибоначчи
     *
     * @param idx
     * @return
     */
    def fibonacci4Index(idx: Int): Int = if (idx == 1 || idx == 2) 1 else fibonacci4Index(idx - 1) + fibonacci4Index(idx - 2)

    def fibonacci(idx: Int): Seq[Int] = (2 to idx).foldLeft(Seq(0, 1)) {
        (acc, i) => acc :+ acc(i - 1) + acc(i - 2)
    }

    lazy val MORSE = Map("A" -> ".-", "B" -> "-...", "C" -> "-.-.", "D" -> "-..", "E" -> ".", "F" -> "..-.",
        "G" -> "--.", "H" -> "....", "I" -> "..", "J" -> ".---", "K" -> "-.-", "L" -> ".-..",
        "M" -> "--", "N" -> "-.", "O" -> "---", "P" -> ".--.", "Q" -> "--.-", "R" -> ".-.",
        "S" -> "...", "T" -> "-", "U" -> "..-", "V" -> "...-", "W" -> ".--", "X" -> "-..-",
        "Y" -> "-.--", "Z" -> "--..")

    def morse(text: String): String = {
        text.map(char => if (MORSE contains char.toUpper.toString) MORSE(char.toUpper.toString) else char).mkString(" ")
    }


    def wordReverse(text: String): String = {
        val splittedText = text.split(" ")
        val res: mutable.StringBuilder = new mutable.StringBuilder
        for (str <- splittedText) {
            val toReverse: mutable.StringBuilder = new mutable.StringBuilder
            for (symbol <- str) {
                if (symbol.isLetter) {
                    toReverse += symbol.toLower
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
