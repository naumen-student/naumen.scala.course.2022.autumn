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
    case DataList.EmptyList => None
    case DataList.NonEmptyList(head, DataList.EmptyList) => Some(head)
    case DataList.NonEmptyList(head, tail) => foldOption(f)(tail)
      .map(f(head, _))
  }



  /**
   * Используя foldOption[T](f: (T, T) => T) реализуйте суммирование всех элементов списка.
   * @return Если список пустой, то 0
   */
  def sum[T : Numeric](list: DataList[T]): T = {
    /**
     * Используйте для суммирования двух чисел любого типа (Int, Long, Double, Float etc)
     */
    val numeric = implicitly[Numeric[T]]

    foldOption(numeric.plus)(list).getOrElse(numeric.zero)
  }

  /**
   * Фильтрация списка. Хвостовая рекурсия
   *
   * @param f - фильтрующее правило (если f(a[i]) == true, то элемент остаётся в списке)
   */
  @tailrec
  private def reverse[T](buffer: DataList[T])(l: DataList[T]): DataList[T] = l match {
    case DataList.NonEmptyList(head, tail) => reverse(DataList.NonEmptyList(head, buffer))(tail)
    case DataList.EmptyList => buffer
  }

  @tailrec
  private def filterImpl[T](f: T => Boolean)(buffer: DataList[T])(l: DataList[T]): DataList[T] =  {
    l match {
      case DataList.NonEmptyList(head, tail) =>
        filterImpl(f)(if (f(head)) DataList.NonEmptyList(head, buffer) else buffer)(tail)
      case DataList.EmptyList => buffer
    }
  }

  final def filter[T](f: T => Boolean): DataList[T] => DataList[T] =
    dl => reverse[T](DataList.EmptyList)(filterImpl(f)(DataList.EmptyList)(dl))

  final def map[A, B](f: A => B): DataList[A] => DataList[B] = {
    case DataList.EmptyList => DataList.EmptyList
    case DataList.NonEmptyList(head, tail) => DataList.NonEmptyList(f(head), map(f)(tail))
  }

  /**
   * Используя композицию функций реализуйте collect. Collect - комбинация filter и map.
   * В качестве фильтрующего правила нужно использовать f.isDefinedAt
   */
  def collect[A, B](f: PartialFunction[A, B]): DataList[A] => DataList[B] = dlist =>
    map[A, B](
      e => f(e)
    )(filter[A](
      a => f.isDefinedAt(a)
    )(dlist))

}