package free
package hashmap

trait Functor[F[+_]] {
  def fmap[A, B](f: A => B): F[A] => F[B]
}
