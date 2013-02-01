package free
package hashmap

sealed trait KeyValueStore[+A] {
  def map[B](f: A => B): KeyValueStore[B] =
    this match {
      case Put(k, v, q) => Put(k, v, f compose q)
      case Get(k, q) => Get(k, f compose q)
      case Del(k, q) => Del(k, f compose q)
    }

}
case class Put[A](k: String, v: String, q: Option[String] => A) extends KeyValueStore[A]
case class Get[A](k: String, q: Option[String] => A) extends KeyValueStore[A]
case class Del[A](k: String, q: Option[String] => A) extends KeyValueStore[A]

object KeyValueStore {
  implicit val KeyValueStoreFunctor: Functor[KeyValueStore] =
    new Functor[KeyValueStore] {
      def fmap[A, B](f: A => B) =
        _ map f
    }
}
