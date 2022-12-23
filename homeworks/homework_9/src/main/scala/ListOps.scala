import scala.annotation.tailrec

object ListOps {
  def foldOption[T](f: (T, T) => T): DataList[T] => Option[T] = {
    case DataList.EmptyList => None
    case DataList.NonEmptyList(head, tail) =>
      Option(if (foldOption(f)(tail).isEmpty) head
      else f(head, foldOption(f)(tail).get))
  }

  def sum[T : Numeric](list: DataList[T]): T = {
    def sumT(a: T, b: T) = implicitly[Numeric[T]].plus(a, b)
    foldOption(sumT)(list).getOrElse(Numeric[T].zero)
  }

  @tailrec
  private def filterImpl[T](f: T => Boolean)(buffer: DataList[T])(l: DataList[T]): DataList[T] = {
    l match {
      case DataList.EmptyList => reverse(buffer)(DataList.EmptyList)
      case DataList.NonEmptyList(head, tail) if f(head) => filterImpl(f)(DataList.NonEmptyList(head, buffer))(tail)
      case DataList.NonEmptyList(_, tail) => filterImpl(f)(buffer)(tail)
    }
  }

  @tailrec
  private def reverse[T](value: DataList[T])(buffer: DataList[T]): DataList[T] = value match {
    case DataList.EmptyList => buffer
    case DataList.NonEmptyList(head, tail) => reverse(tail)(DataList.NonEmptyList(head, buffer))
  }

  final def filter[T](f: T => Boolean): DataList[T] => DataList[T] = filterImpl(f)(DataList.EmptyList)

  final def map[A, B](f: A => B): DataList[A] => DataList[B] = {
    case DataList.EmptyList => DataList.EmptyList
    case DataList.NonEmptyList(head, tail) => DataList.NonEmptyList(f(head), map(f)(tail))
  }

  def collect[A, B](f: PartialFunction[A, B]): DataList[A] => DataList[B] = map(f).compose(filter(f.isDefinedAt))
}