package free
package hashmap

case class KeyValueStoreInterpreter[+A](free: Free[KeyValueStore, A]) {
  def map[X](f: A => X): KeyValueStoreInterpreter[X] =
    KeyValueStoreInterpreter(free map f)

  def flatMap[X](f: A => KeyValueStoreInterpreter[X]): KeyValueStoreInterpreter[X] =
    KeyValueStoreInterpreter(free flatMap (f(_).free))

  final def resume: Resume[KeyValueStore, A] =
    free.resume

  /** Changes the configuration functor by the given natural transformation. */
  def hom[G[+_]](f: KeyValueStore ~> G)(implicit G: Functor[G]): Free[G, A] =
    free mapSuspension f

  // CAUTION
  // Unsafe operation. Run once only.
  @annotation.tailrec
  final def runJHashMap(m: java.util.HashMap[String, String]): A =
    resume match {
      case Cont(Put(k, v, q)) =>
        KeyValueStoreInterpreter(q(Option(m put (k, v)))) runJHashMap m
      case Cont(Get(k, q)) =>
        KeyValueStoreInterpreter(q(Option(m get k))) runJHashMap m
      case Cont(Del(k, q)) =>
        KeyValueStoreInterpreter(q(Option(m remove k))) runJHashMap m
      case Term(a) =>
        a
    }
}

object KeyValueStoreInterpreter {
  def put[A](k: String, v: String): KeyValueStoreInterpreter[Option[String]] =
    KeyValueStoreInterpreter(More(Put(k, v, Done(_))))

  def get[A](k: String): KeyValueStoreInterpreter[Option[String]] =
    KeyValueStoreInterpreter(More(Get(k, Done(_))))

  def del[A](k: String): KeyValueStoreInterpreter[Option[String]] =
    KeyValueStoreInterpreter(More(Del(k, Done(_))))
}
