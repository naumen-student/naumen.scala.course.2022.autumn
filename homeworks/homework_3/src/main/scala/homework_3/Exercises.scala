package homework_3

object Exercises {
    /**
     * Задание №1
     */
    def prettyBooleanFormatter1(x: Any): String = {
        if (x.isInstanceOf[Boolean]) {
            if (x.asInstanceOf[Boolean]) "правда" else "ложь"
        }
        else x.toString
    }

    def prettyBooleanFormatter2(x: Any): String = x match {
        case true => "правда"
        case false => "ложь"
        case y => y.toString
    }

    def prettyBooleanFormatter3(x: Any): String = {
        val bool: Any = true
        if (bool.getClass == x.getClass) {
            if (x.asInstanceOf[Boolean]) "правда" else "ложь"
        }
        else x.toString
    }


    /**
     * Задание №2
     */
    def max1(xs: Seq[Int]): Int = xs.max

    def max2(xs: Seq[Int]): Seq[Int] = {
        if (xs.isEmpty) Seq.empty
        else Seq(xs.max)
    }

    def max3(xs: Seq[Int]): Option[Int] = {
        if (xs.isEmpty) Option.empty
        else Option(xs.max)
    }

    /**
     * Задание №3
     */
    def sumIntegers[CollectionType <: Iterable[Int]](xs: CollectionType): Int = xs.sum


    class MyType(x: Int, y: Int) extends Iterable[Int] {
        override def iterator: Iterator[Int] = {
            Iterator(x, y)
        }
    }

    def sum1(x: Int, y: Int): Int = sumIntegers(List(x, y))
    def sum2(x: Int, y: Int): Int = sumIntegers(Seq(x, y))
    def sum3(x: Int, y: Int): Int = sumIntegers(new MyType(x, y))
}
