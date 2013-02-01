package free
package hashmap

case class KeyValueStoreInterpreter[+A](free: Free[KeyValueStore, A]) {
  def map[X](f: A => X): KeyValueStoreInterpreter[X] =
    KeyValueStoreInterpreter(free map f)

    def flatMap[X](f: A => KeyValueStoreInterpreter[X]): KeyValueStoreInterpreter[X] =
      KeyValueStoreInterpreter(free flatMap (f(_).free))

    final def resume: Resume[KeyValueStore, A] =
      free.resume
}