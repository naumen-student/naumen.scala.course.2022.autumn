import scala.util.Try

object Exercises {
  trait Read[T] {
    def read(value: String): Either[String, T]
  }


  object Read {
    def read[T](str: String)(implicit reader: Read[T]): Either[String, T] = reader.read(str)
  }

  implicit class ReadOptions(private val value: String) {
    def read[T](implicit reader: Read[T]): Either[String, T] = reader.read(value)
  }

  implicit val strReader: Read[String] = Right(_)
  implicit val intReader: Read[Int] = (value: String) => Try(value.toInt).toOption.toRight(value)

  val pattern = """Some\((\S+)\)""".r

  implicit def optionReader[T: Read]: Read[Option[T]] = {
    case "None" => Right(None)
    case pattern(value) => value.read[T].map(Some(_))
    case other => Left(other)
  }
}