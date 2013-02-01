package free
package hashmap
package example1

sealed trait KeyValueStoreEffect[+A] {
  def map[B](f: A => B): KeyValueStoreEffect[B] =
    this match {
      case KeyValueStoreNoEffect(e) => KeyValueStoreNoEffect(e map f)
      case KeyValueStoreEffectPrintln(s, a) => KeyValueStoreEffectPrintln(s, f(a))
      case KeyValueStoreEffectPutPrintln(k, v, a) => KeyValueStoreEffectPutPrintln(k, v, f(a))
    }
}
case class KeyValueStoreNoEffect[+A](e: KeyValueStoreInterpreter[A]) extends KeyValueStoreEffect[A]
case class KeyValueStoreEffectPrintln[+A](s: String, a: A) extends KeyValueStoreEffect[A]
case class KeyValueStoreEffectPutPrintln[+A](k: String, v: String, a: A) extends KeyValueStoreEffect[A]

object KeyValueStoreEffect {
  implicit val KeyValueStoreEffectFunctor: Functor[KeyValueStoreEffect] =
    new Functor[KeyValueStoreEffect] {
      def fmap[A, B](f: A => B) =
        _ map f
    }
}
