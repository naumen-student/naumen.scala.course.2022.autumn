
trait Monad[F[_]] {

    def pure[A](a: A): F[A]

    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = flatMap(fa) {(a: A) => flatMap(fb) {(b: B) => pure(f(a, b))}}

    def sequence[A](fas: List[F[A]]): F[List[A]] = if (fas.isEmpty) ans else sequence(fas.tail, map2(ans, fas.head)((a, b) => a :+ b))

    def compose[A, B, C](f: A => F[B])(g: B => F[C]): A => F[C] = ans => flatMap(f(ans))(g)
}

trait Functor[F[_]] {
    def map[A, B](a: F[A])(f: A => B): F[B]
}

object Functor {
    def functorFromMonad[F[_]](M: Monad[F]): Functor[F] = new Functor[F] {
        def map[A, B](a: F[A])(f: A => B): F[B] = M.flatMap(a)(r => M.pure(f(r)))
    }
}
