import scala.util.Try
import scala.util.matching.Regex

object Exercises {
  implicit class ReadOptions(private val value: String) {
    def read[T](implicit reader: Read[T]): Either[String, T] = reader.read(value)
  }

  trait Read[T] {
    def read(value: String): Either[String, T]
  }

  object Read {
    def read[T](string: String)(implicit reader: Read[T]): Either[String, T] = reader.read(string)
  }

  implicit val strR: Read[String] = (value: String) => Right(value)
  implicit val intR: Read[Int] = (value: String) => Try(value.toInt).toEither.fold(l => Left(l.toString), r => Right(r))

  val pat: Regex = "Some\\((.*)\\)".r

  implicit def optionReader[T: Read]: Read[Option[T]] = {
    case "None" => Right(None)
    case pat(value) => value.read[T].map(Some(_))
    case other => Left($other)
  }
}
