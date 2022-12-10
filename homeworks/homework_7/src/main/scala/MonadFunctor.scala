
private object Implicits {
  implicit class MonadOps[A, F[_] : Monad](private val fa: F[A]) {
    def map[B](f: A => B): F[B] = Monad[F].map(fa)(f)

    def >>=[B](f: A => F[B]): F[B] = Monad[F].flatMap(fa)(f)
  }
}

trait Monad[F[_]] {
  self =>

  import Implicits._

  implicit val monad = self

  def pure[A](a: A): F[A]

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def map[A, B](fa: F[A])(f: A => B) = flatMap(fa)(f.andThen(pure))

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = fa >>= (a => fb.map(f(a, _)))

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    fas.foldLeft(pure(List.empty[A]))((acc, fa) => fa >>= (a => acc.map(_.prepended(a)))).map(_.reverse)

  def compose[A, B, C](f: A => F[B])(g: B => F[C]): A => F[C] = f.andThen(_ >>= g)

}

object Monad {
  def apply[F[_] : Monad]: Monad[F] = implicitly[Monad[F]]
}

trait Functor[F[_]] {
  def map[A, B](a: F[A])(f: A => B): F[B]
}

object Functor {
  def functorFromMonad[F[_]](M: Monad[F]): Functor[F] = new Functor[F] {
    def map[A, B](a: F[A])(f: A => B): F[B] = M.map(a)(f)
  }
}
