package homework_3

import cats.Monoid

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
  def prettyBooleanFormatter1(x: Any): String = x match {
    case true => "правда"
    case false => "ложь"
    case t => t.toString
  }

  def prettyBooleanFormatter2(x: Any): String =
    x.toString.toBooleanOption.fold(x.toString)(b => if (b) "правда" else "ложь")

  def prettyBooleanFormatter3(x: Any): String =
    if (x.isInstanceOf[Boolean]) {
      val b = x.asInstanceOf[Boolean]
      if (b) "правда" else "ложь"
    }
    else x.toString


  /**
   * Задание №2
   * Реализуйте функцию нахождения максимального числа в переданной коллекции интов (можно использовать все методы стандартной библиотеки).
   *
   * Реализуйте функцию тремя разными способами, отличающимися тем как функция себя ведет на пустой коллекции.
   * Обратите внимание на возвращаемые типы.
   */
  def max1(xs: Seq[Int]): Int = xs.max

  def max2(xs: Seq[Int]): Seq[Int] = {
    val m = xs.maxOption
    m.fold(Seq.empty[Int])(m => Seq.iterate(m, xs.count(_ == m))(_ => m))
  }

  def max3(xs: Seq[Int]): Option[Int] = xs.maxOption

  /**
   * Задание №3
   * Допустим дана функция sumIntegers, которая умеет суммировать числа.
   */
  def sumIntegers[CollectionType <: Iterable[Int]](xs: CollectionType): Int = xs.sum

  /**
   * Реализуйте на основе нее 3 варианта суммирования 2х чисел, отличающиеся способом передачи этих 2х чисел в функцию sumIntegers.
   * Как минимум одна из реализаций должна использовать тип данных (класс) написанный вами самостоятельно.
   */

  sealed abstract class SList[A: Monoid] extends Iterable[A] { self =>
    def prepended(v: A): Nempty[A] = Nempty(v, self)
  }

  object SList {
    def empty[A: Monoid]: SList[A] = Empty[A]
    def apply[A: Monoid](values: A*): SList[A] = values.foldLeft(SList.empty[A]){case (acc, e) => acc.prepended(e)}

  }

  case class Nempty[A: Monoid](value: A, rest: SList[A]) extends SList[A] {
    override def iterator: Iterator[A] = Iterator.iterate(value, 1)(identity) ++ rest.iterator
  }

  case class Empty[A: Monoid]() extends SList[A] {
    override def iterator: Iterator[A] = Iterator.empty[A]
  }

  def sum1(x: Int, y: Int): Int = sumIntegers(Nempty(x, Nempty(y, Empty[Int]())))

  def sum2(x: Int, y: Int): Int = sumIntegers(List(x, y))

  def sum3(x: Int, y: Int): Int = sumIntegers(Set(x, y))

}
