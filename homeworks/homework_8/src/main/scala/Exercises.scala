import scala.util.{Failure, Success, Try}
object Exercises {
  trait Read[T] {
    def read(input: String): Either[String, T]
  }

  object Read {
    def read[T: Read](input: String)(implicit reader: Read[T]): Either[String, T] = reader.read(input)
  }

  implicit class optionRead(input: String) {
    def read[T](implicit reader: Read[T]): Either[String, T] = reader.read(input)
  }

  implicit val readInt: Read[Int] = (input: String) => {
    Try(input.toInt) match {
      case Success(value) => Right(value)
      case Failure(exception) => Left("Incorrect Int input")
    }
  }

  implicit val readString: Read[String] = Right(_)

  implicit def readOption[T](implicit reader: Read[T]): Read[Option[T]] = (input: String) => {
      val somePattern = {
        "Some\\((.*)\\)".r
      }
      input match {
        case "None" => Right(None)
        case somePattern(some) => some.read[T].map(Some(_))
        case _ => Left("Incorrect Option input")
      }
  }


}
