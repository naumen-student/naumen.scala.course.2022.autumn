trait Monad[F[_]] extends Functor[F] {

  def pure[A](a: A): F[A]

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => pure(f(a)))

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = flatMap(fa)(a => map(fb)(f(a, _)))

  def sequence[A](fas: List[F[A]]): F[List[A]] = map {
    fas.foldLeft(pure(List.empty[A])) { case (prev, next) => map2(prev, next) { case (list, el) => el :: list } }
  }(_.reverse)

  def compose[A, B, C](f: A => F[B])(g: B => F[C]): A => F[C] = a => flatMap(f(a))(g)
}

trait Functor[F[_]] {
  def map[A, B](a: F[A])(f: A => B): F[B]
}

object Functor {
  def functorFromMonad[F[_]](M: Monad[F]): Functor[F] = M
}
