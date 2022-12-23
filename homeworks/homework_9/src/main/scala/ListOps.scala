import DataList.{EmptyList, NonEmptyList}

import scala.annotation.tailrec

object ListOps {

  /**
   * Функция fold "сворачивает" список из Т в один элемент типа Т.
   * Если в списке лишь один элемент, то он и вернётся, два - вернётся результат применения f к этим элементам,
   * больше двух - результат применения к f(f(f(...), a[i - 1]), a[i])
   * @param f функция свёртывания. Применяется попарно к предыдущему результату применения и i-ому элементу списка
   * @return None - если список пустой
   */
  def foldOption[T](f: (T, T) => T): DataList[T] => Option[T] = {
    case EmptyList => None
    case NonEmptyList(head, tail) => tail match {
      case EmptyList => Some(head)
      case NonEmptyList(innerHead, innerTail) => foldOption(f)(NonEmptyList(f(head, innerHead), innerTail))
    }
  }


  /**
   * Используя foldOption[T](f: (T, T) => T) реализуйте суммирование всех элементов списка.
   * @return Если список пустой, то 0
   */
  def sum[T : Numeric](list: DataList[T]): T = {
    /**
     * Используйте для суммирования двух чисел любого типа (Int, Long, Double, Float etc)
     */
    def sumT(a: T, b: T) = implicitly[Numeric[T]].plus(a, b)
    foldOption(sumT)(list) match {
      case Some(result) => result
      case None => Numeric[T].zero
    }
  }

  /**
   * Фильтрация списка. Хвостовая рекурсия
   * @param f - фильтрующее правило (если f(a[i]) == true, то элемент остаётся в списке)
   */
  @tailrec
  private def filterImpl[T](f: T => Boolean)(buffer: DataList[T])(l: DataList[T]): DataList[T] = {
    l match {
      case EmptyList =>  reverse(buffer)(EmptyList)
      case NonEmptyList(head, tail) =>
        if (f(head)) filterImpl(f)(NonEmptyList(head, buffer))(tail)
        else filterImpl(f)(buffer)(tail)
    }
  }

  @tailrec
  private def reverse[T](l: DataList[T])(reversedList: DataList[T]): DataList[T] = {
    l match {
      case NonEmptyList(head, tail) => reverse(tail)(NonEmptyList(head, reversedList))
      case EmptyList => reversedList
    }
  }


  final def filter[T](f: T => Boolean): DataList[T] => DataList[T] = filterImpl(f)(DataList.EmptyList)

  final def map[A, B](f: A => B): DataList[A] => DataList[B] = {
    case DataList.EmptyList => DataList.EmptyList
    case DataList.NonEmptyList(head, tail) => DataList.NonEmptyList(f(head), map(f)(tail))
  }

  /**
   * Используя композицию функций реализуйте collect. Collect - комбинация filter и map.
   * В качестве фильтрующего правила нужно использовать f.isDefinedAt
   */
  def collect[A, B](f: PartialFunction[A, B]): DataList[A] => DataList[B] = result => map(f)(filter(f.isDefinedAt)(result))

}