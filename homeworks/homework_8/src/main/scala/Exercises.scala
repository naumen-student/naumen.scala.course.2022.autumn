import scala.util.{Failure, Success, Try}

object Exercises {
  trait Read[T] {
    def read(str: String): Either[String, T];
  }

  implicit class ReadImplicit(string: String) {
    def read[T](implicit read: Read[T]): Either[String, T] = read.read(string)
  }

  object Read {
    def read[T](string: String)(implicit read: Read[T]): Either[String, T] = read.read(string)

    implicit object readInt extends Read[Int] {
      override def read(str: String): Either[String, Int] = {
        Try(str.toInt) match {
          case Success(x) => Right(x)
          case Failure(exception) => Left(exception.getMessage);
        }
      }
    }

    implicit object readString extends Read[String] {
      override def read(str: String): Either[String, String] = Right(str);
    }

    val someRegex = """Some\((\S+)\)""".r
    val error = "Unable to read";

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