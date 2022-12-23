import DataList.{EmptyList, NonEmptyList}

import scala.annotation.tailrec

object ListOps {

  /**
   * Функция fold "сворачивает" список из Т в один элемент типа Т.
   * Если в списке лишь один элемент, то он и вернётся, два - вернётся результат применения f к этим элементам,
   * больше двух - результат применения к f(f(f(...), a[i - 1]), a[i])
   *
   * @param f функция свёртывания. Применяется попарно к предыдущему результату применения и i-ому элементу списка
   * @return None - если список пустой
   */
  def foldOption[T](f: (T, T) => T): DataList[T] => Option[T] = {
    @tailrec
    def go(list: DataList[T], acc: T): T = list match {
      case NonEmptyList(head, tail) => go(tail, f(acc, head))
      case EmptyList => acc
    }
    {
      case EmptyList => None
      case NonEmptyList(head, tail) => Some(go(tail, head))
    }
  }


  /**
   * Используя foldOption[T](f: (T, T) => T) реализуйте суммирование всех элементов списка.
   *
   * @return Если список пустой, то 0
   */
  def sum[T: Numeric](list: DataList[T]): T = {
    /**
     * Используйте для суммирования двух чисел любого типа (Int, Long, Double, Float etc)
     */
    def sumT(a: T, b: T) = Numeric[T].plus(a, b)

    foldOption(sumT)(list).getOrElse(Numeric[T].zero)
  }

  def reverse[A]: DataList[A] => DataList[A] = {
    @tailrec
    def go(acc: DataList[A])(dataList: DataList[A]): DataList[A] =
      dataList match {
        case NonEmptyList(head, tail) => go(NonEmptyList(head, acc))(tail)
        case DataList.EmptyList => acc
      }

    go(DataList.EmptyList)
  }

  /**
   * Фильтрация списка. Хвостовая рекурсия
   *
   * @param f - фильтрующее правило (если f(a[i]) == true, то элемент остаётся в списке)
   */
  final def filter[T](f: T => Boolean): DataList[T] => DataList[T] = {
    @tailrec
    def filterImpl(acc: DataList[T])(l: DataList[T]): DataList[T] =
      l match {
        case NonEmptyList(head, tail) if f(head) => filterImpl(NonEmptyList(head, acc))(tail)
        case NonEmptyList(_, tail) => filterImpl(acc)(tail)
        case DataList.EmptyList => acc
      }

    reverse compose filterImpl(DataList.EmptyList)
  }

  final def map[A, B](f: A => B): DataList[A] => DataList[B] = {
    case DataList.EmptyList => DataList.EmptyList
    case DataList.NonEmptyList(head, tail) => DataList.NonEmptyList(f(head), map(f)(tail))
  }

  /**
   * Используя композицию функций реализуйте collect. Collect - комбинация filter и map.
   * В качестве фильтрующего правила нужно использовать f.isDefinedAt
   */
  def collect[A, B](f: PartialFunction[A, B]): DataList[A] => DataList[B] = map(f) compose filter(f.isDefinedAt)

}