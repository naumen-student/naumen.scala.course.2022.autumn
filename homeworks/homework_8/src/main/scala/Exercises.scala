import scala.util.Try

object Exercises {
  object Read {
    def read[T](s: String)(implicit r: Read[T]): Either[String, T] = r.read(s)
  }

  trait Read[T] {
    def read(s: String): Either[String, T]
  }

  implicit class ReadOps(s: String) {
    def read[T](implicit r: Read[T]): Either[String, T] = r.read(s)
  }

  implicit val stringRead: Read[String] = (s: String) => Right(s)

  implicit val intRead: Read[Int] = (s: String) => s.toIntOption.toRight(s)

  implicit def optionRead[T](implicit r: Read[T]): Read[Option[T]] = {
    case "None" => Right(None)
    case s"Some($remaining)" => remaining.read[T].map(Some(_))
    case s => Left(s)
  }
}