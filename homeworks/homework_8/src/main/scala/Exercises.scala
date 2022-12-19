import scala.util.Try
import scala.util.matching.Regex
object Exercises {
  trait Read[T] {
    def read(str: String): Either[String, T]
  }

  implicit class ReadOptions(private val value: String) {
    def read[T](implicit reader: Read[T]): Either[String, T] = reader.read(value)
  }

  object Read {
    def read[T](string: String)(implicit read: Read[T]): Either[String, T] = read.read(string)
  }

  implicit val strReader: Read[String] = (str: String) => Right(str)

  implicit val intReader: Read[Int] = (str: String) => {
    try {
      Right(str.toInt)
    } catch {
      case e: NumberFormatException => Left(s"$str is not in correct format")
    }
  }
  val someRegex: Regex = """Some\((\S+)\)""".r
  implicit def optionReader[T: Read]: Read[Option[T]] = {
    case "None" => Right(None)
    case someRegex(value) => value.read[T].map(Some(_))
    case x => Left(s"$x can't be read")
  }
}

