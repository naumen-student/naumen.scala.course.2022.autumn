import scala.util.Try
object Exercises {
  object Read {
    def read[T](string: String)(implicit read: Read[T]): Either[String, T] = read.read(string)
  }

  trait Read[T] {
    def read(string: String): Either[String, T]
  }

  implicit class ReadOps(string: String) {
    def read[T](implicit read: Read[T]): Either[String, T] = read.read(string)
  }

  implicit val stringRead: Read[String] = (string: String) => Right(string)

  implicit val intRead: Read[Int] = (string: String) => string.toIntOption.toRight(string)

  implicit def optionRead[T](implicit read: Read[T]): Read[Option[T]] = {
    case "None" => Right(None)
    case s"Some($remaining)" => remaining.read[T].map(Some(_))
    case string => Left(string)
  }
}
