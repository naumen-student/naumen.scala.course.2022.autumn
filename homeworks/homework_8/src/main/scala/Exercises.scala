import scala.Right
import scala.util.Try
object Exercises {
  trait Read[A] {
    def read(value: String): Either[String, A]
  }
  object Read {
    def read[A] (
                  value: String
                ) (
      implicit reader: Read[A]
    ) : Either[String, A] =
      reader.read(value)
  }

  implicit def stringReader : Read[String] = (value: String) => Right(value)

  implicit def intReader: Read[Int] = (value: String) => {
    Try(value.toInt).toEither.fold(l => Left(l.toString), r => Right(r))
  }
  implicit def optReader[A](implicit reader: Read[A]): Read[Option[A]] = (value: String) => {
    val somePattern = "Some\\((.*)\\)".r
    val nonePattern = "None".r

    value match {
      case somePattern(word) => word.read[A].map(Some(_))
      case nonePattern() => Right(None)
      case other => Left(other)
    }
  }

  implicit class ReadOps(value: String) {
    def read[A](implicit reader: Read[A]): Either[String, A] = {
      reader.read(value)
    }
  }
}
