
trait Monad[F[_]] {

  def pure[A](a: A): F[A]

  def flatMap[C, D](fa: F[C])(f: C => F[D]): F[D]

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = flatMap(fa)((a: A) => {
    flatMap(fb)((b: B) => {
      pure(f(a, b))
    })
  })

  def sequence[A](fas: List[F[A]]): F[List[A]] = {
    var list = pure(List[A]())
    for (elem <- fas) {
      list = map2(list, elem)((a, b) => a :+ b)
    }
    list
  }

  def compose[A, B, C](f: A => F[B])(g: B => F[C]): A => F[C] = (a: A) => flatMap(flatMap(pure(a))(f))(g)
}

trait Functor[F[_]] {
  def map[A, B](a: F[A])(f: A => B): F[B]
}

object Functor {
  def functorFromMonad[F[_]](M: Monad[F]): Functor[F] = new Functor[F] {
    def map[A, B](a: F[A])(f: A => B): F[B] = M.flatMap(a)(x => M.pure(f(x)))
  }
}
