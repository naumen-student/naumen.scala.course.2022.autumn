import scala.util.Try
import RichEither._

object Exercises {
  trait Read[T] {
    def read(in: String): Either[String, T]
  }

  object Read {
    def apply[T: Read]: Read[T] = implicitly

    def read[T: Read](in: String): Either[String, T] = Read[T].read(in)

    object Instance {
      implicit val StringRead: Read[String] = (in: String) => Right(in)
      implicit val IntRead: Read[Int]       = (in: String) => Try(in.toInt).toEither.mapLeft(th => th.getMessage)
      implicit def OptionRead[TT: Read]: Read[Option[TT]] =
        (in: String) =>
          if (in == NoneString) Right(None)
          else {
            SomeRegex
              .findFirstMatchIn(in)
              .map(_.group(1))
              .flatMap(Read[TT].read(_).toOption)
              .toRight("Error parse Option")
              .map(Some(_))
          }

      private val NoneString = "None"
      private val SomeRegex  = "^Some\\((.*)\\)$".r
    }

    implicit class ReadSyntax(e: String) {
      def read[A: Read]: Either[String, A] =
        Read[A].read(e)
    }
  }
}

object RichEither {
  implicit class EitherOps[A, B](either: Either[A, B]) {
    def mapLeft[A1](lf: A => A1): Either[A1, B] = {
      either match {
        case Right(value) => Right(value)
        case Left(value)  => Left(lf(value))
      }
    }
  }
}
