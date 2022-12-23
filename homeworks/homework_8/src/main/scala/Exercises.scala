import scala.util.{Success, Try, Failure}
import scala.util.matching.Regex

object Exercises {
  trait Read[T] {
    def read(str: String): Either[String, T]
  }

  object Read {
    def read[T](str: String)(implicit read: Read[T]): Either[String, T] = read.read(str)
  }

  implicit class ReadOps(str: String) {
    def read[T](implicit read: Read[T]): Either[String, T] = read.read(str)
  }

  implicit def stringRead: Read[String] = (str: String) => Right(str)

  implicit def intRead: Read[Int] = (str: String) => Try(str.toInt) match {
    case Success(value) => Right(value)
    case Failure(_) => Left("Failed to read a number")
  }

  implicit def optionRead[T: Read]: Read[Option[T]] = {
    case "None" => Right(None)
    case str => """Some\((.+)\)""".r.findFirstMatchIn(str) match {
      case Some(opt) => Read.read[T](opt.group(1)) match {
        case Right(value) => Right(Some(value))
        case Left(_) => Left("Failed to read option value")
      }
      case None => Left("Incorrect format")
    }
  }
}
