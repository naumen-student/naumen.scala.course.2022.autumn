import scala.util.Try

object Exercises {
  trait Read[T] {
    def read(s: String): Either[String, T]
  }

  object Read {
    def read[T: Read](v: String)(implicit r: Read[T]): Either[String, T] = r.read(v)
  }

  implicit class ReadOption(v: String) {
    def read[T](implicit reader: Read[T]): Either[String, T] = reader.read(v)
  }

  implicit val readInt: Read[Int] = (v: String) => v.toIntOption.toRight(v)

  implicit val readString: Read[String] = Right(_)

  implicit def readOption[T: Read]: Read[Option[T]] = {
    case "None" => Right(None)
    case s@s"Some($v)" => v.read[T].map(Some(_))
    case other => Left(other)
  }
}