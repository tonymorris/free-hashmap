package free
package hashmap

trait ~>[F[_], G[_]] {
  def apply[A](a: F[A]): G[A]
}
