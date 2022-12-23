import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}

object Exercises {
  trait Read[T] {
    def read(string: String): Either[String, T]
  }

  object Read {
    def read[T: Read](value: String)(implicit reader: Read[T]): Either[String, T] = reader.read(value)
  }

  implicit class OptReader(value: String) {
    def read[T](implicit reader: Read[T]): Either[String, T] = reader.read(value)
  }

  private val someRegex: Regex = """Some\((\S+)\)""".r

  implicit val intReader: Read[Int] = (value: String) => Try(value.toInt).toOption.toRight(value)

  implicit val strReader: Read[String] = Right(_)

  implicit def readOption[T: Read]: Read[Option[T]] = {
    case someRegex(value) => value.read[T].map(Some(_))
    case "None" => Right(None)
    case other => Left(other)
  }
}
