import scala.util.Try

object Exercises {

  trait Read[T] {
    def read(string: String): Either[String, T]
  }

  object Read {
    def read[T: Read](value: String)(implicit reader: Read[T]): Either[String, T] = reader.read(value)
  }

  implicit class OptionReader(value: String) {
    def read[T](implicit reader: Read[T]): Either[String, T] = reader.read(value)
  }

  implicit val intReader: Read[Int] = (value: String) => Try(value.toInt).toOption.toRight(value)

  implicit val stringReader: Read[String] = Right(_)

  val rgx = """Some\((\S+)\)""".r

  implicit def readOption[T: Read]: Read[Option[T]] = {
    case "None" => Right(None)
    case rgx(value) => value.read[T].map(Some(_))
    case other => Left(other)
  }
}