package homework_3

object Exercises {


    /**
     * Задание №1
     * Реализуйте функцию, которая принимает любой тип и преобразует его в строку.
     * Для всех типов кроме Boolean достаточно воспользоваться стандартной функцией .toString.
     * Для типа Boolean сделайте особое преобразование: true -> "правда", false -> "ложь".
     *
     * Реализуйте функцию тремя разными способами, отличающимися тем, как определяется какой тип имеет значение переданное в аргументе. 
     * Определение типа необходимо для реализации специальной логики работы с Boolean значениями, которая описана в условии выше.
     */
    def prettyBooleanFormatter1(x: Any): String = {
        var result = x.toString
        if (x.isInstanceOf[Boolean]) {
            result = if (x.asInstanceOf[Boolean]) "правда" else "ложь"
        }
        result
    }

    def prettyBooleanFormatter2(x: Any): String = {
        var result = x.toString
        println(x.getClass)
        if (x.getClass == classOf[java.lang.Boolean]) {
            result = if (x.asInstanceOf[Boolean]) "правда" else "ложь"
        }
        result
    }

    def prettyBooleanFormatter3(x: Any): String = x match {
        case value: Boolean => if (value) "правда" else "ложь"
        case _ => x.toString
    }


    /**
     * Задание №2
     * Реализуйте функцию нахождения максимального числа в переданной коллекции интов (можно использовать все методы стандартной библиотеки).
     *
     * Реализуйте функцию тремя разными способами, отличающимися тем как функция себя ведет на пустой коллекции. 
     * Обратите внимание на возвращаемые типы.
     */
    def max1(xs: Seq[Int]): Int = if (xs.isEmpty) throw new EmptySeqException("empty seq") else xs.max

    def max2(xs: Seq[Int]): Seq[Int] = if (xs.isEmpty) xs else Seq(xs.max)

    def max3(xs: Seq[Int]): Option[Int] = if (xs.isEmpty) None else Some(xs.max)

    class EmptySeqException(message: String) extends Exception(message)

    /**
     * Задание №3
     * Допустим дана функция sumIntegers, которая умеет суммировать числа.
     */
    def sumIntegers[CollectionType <: Iterable[Int]](xs: CollectionType): Int = xs.sum

    /**
     * Реализуйте на основе нее 3 варианта суммирования 2х чисел, отличающиеся способом передачи этих 2х чисел в функцию sumIntegers.
     * Как минимум одна из реализаций должна использовать тип данных (класс) написанный вами самостоятельно.
     */ 
    def sum1(x: Int, y: Int): Int = sumIntegers(Seq(x, y))
    def sum2(x: Int, y: Int): Int = sumIntegers(List(x, y))
    def sum3(x: Int, y: Int): Int = sumIntegers(new MyCollection(x, y))

    class MyCollection(x: Int, y: Int) extends Iterable[Int] {
        override def iterator: Iterator[Int] = Iterator(x, y)
    }
}
