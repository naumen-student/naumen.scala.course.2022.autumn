import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}
object Exercises {
  trait Read[T] {
    def read(str : String): Either[String, T]
  }

  implicit class ReadImplicit(str: String) {
    def read[T](implicit read: Read[T]): Either[String, T] = read.read(str)
  }

  object Read {
    def read[T](str: String)(implicit read: Read[T]): Either[String, T] = read.read(str)

    implicit object IntReader extends Read[Int] {
      override def read(str: String): Either[String, Int] = {
        Try(str.toInt) match {
          case Success(x) => Right(x)
          case Failure(exception) => Left(exception.getMessage);
        }
      }
    }
//
    implicit val StringReader: Read[String] = Right(_)

    val someRegex: Regex = "^Some\\((.*)\\)$".r
    val error = "Unable to read"

    implicit def readOption[T: Read]: Read[Option[T]] = {
      case "None" => Right(None)
      case someRegex(value) => value.read[T] match {
        case Right(x) => Right(Some(x))
        case Left(_) => Left(error)
      }
      case _ => Left(error)
    }
  }
}
