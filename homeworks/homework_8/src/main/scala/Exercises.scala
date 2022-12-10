import scala.reflect.ClassTag
import scala.util.Try

object Exercises {
  implicit class ReadOps(private val value: String) extends AnyVal {
    def read[A: Read] = Read.read[A](value)
  }

  trait Read[A] {
    def read(s: String): Either[String, A]
  }

  object Read {
    def apply[A: Read]: Read[A] = implicitly[Read[A]]

    def read[A: Read](a: String): Either[String, A] = Read[A].read(a)

  }

  implicit val intRead: Read[Int] = _.toIntOption.toRight("non-int provided")
  implicit val stringRead: Read[String] = Right(_)

  implicit def optionRead[T: Read]: Read[Option[T]] = {
    case "None" => Right(None)
    case s@s"Some($value)" => value.read[T].map(Some(_))
    case other => Left(other)
  }
}