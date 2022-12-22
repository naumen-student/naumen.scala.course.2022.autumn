import scala.util.Try
import scala.util.matching.Regex
object Exercises {
  trait Read[T] {
    def read(value: String): Either[String, T]
  }
  implicit class ReadOptions(private val value: String) {
    def read[T](implicit reader: Read[T]): Either[String, T] = reader.read(value)
  }
  object Read {
    def read[T](string: String)(implicit reader: Read[T]): Either[String, T] = reader.read(string)
  }

  implicit val strReader: Read[String] = (value: String) => Right(value)
  implicit val intReader: Read[Int] = (value: String) => Try(value.toInt).toEither.fold(l => Left(l.toString), r => Right(r))

  val somePattern: Regex = "Some\\((.*)\\)".r
  implicit def optionReader[T: Read]: Read[Option[T]] = {
    case somePattern(value) => value.read[T].map(Some(_))
    case "None" => Right(None)
    case p => Left(s"$p can't be read")
  }
}
