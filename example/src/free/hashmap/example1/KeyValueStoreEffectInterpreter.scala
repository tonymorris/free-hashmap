package free
package hashmap
package example1

case class RichHashMap[K, V](m: java.util.HashMap[K, V]) {
  def putPrintln(k: K, v: V) = {
    println("putting ... before: " + m.get(k))
    m.put(k, v)
    println("put " + k + " to " + v)
  }
}

object RichHashMap {
  implicit def ToRichHashMap[K, V](m: java.util.HashMap[K, V]) =
    RichHashMap(m)
}

import RichHashMap._

case class KeyValueStoreEffectInterpreter [+A](free: Free[KeyValueStoreEffect, A]) {
  def map[X](f: A => X): KeyValueStoreEffectInterpreter [X] =
    KeyValueStoreEffectInterpreter (free map f)

  def flatMap[X](f: A => KeyValueStoreEffectInterpreter [X]): KeyValueStoreEffectInterpreter [X] =
    KeyValueStoreEffectInterpreter (free flatMap (f(_).free))

  final def resume: Resume[KeyValueStoreEffect, A] =
    free.resume

  // CAUTION
  // Unsafe operation. Run once only.
  // @annotation.tailrec
  final def runJHashMap(m: java.util.HashMap[String, String]): A =
    resume match {
      case Cont(KeyValueStoreNoEffect(e)) =>
        KeyValueStoreEffectInterpreter (e runJHashMap m) runJHashMap m
      case Cont(KeyValueStoreEffectPrintln(s, a)) =>
        KeyValueStoreEffectInterpreter ({
          println(s)
          a
        }) runJHashMap m
      case Cont(KeyValueStoreEffectPutPrintln(k, v, a)) =>
        KeyValueStoreEffectInterpreter ({
          m.putPrintln(k, v)
          a
        }) runJHashMap m
      case Term(a) =>
        a
    }
}

object KeyValueStoreEffectInterpreter {
  implicit val KeyValueStoreEffectInterpreterFunctor: Functor[KeyValueStoreEffectInterpreter] =
    new Functor[KeyValueStoreEffectInterpreter] {
      def fmap[A, B](f: A => B) =
        _ map f
    }

  // Used internally (see below).
  private def lift[A](x: KeyValueStoreInterpreter[A]): KeyValueStoreEffectInterpreter[A] =
    KeyValueStoreEffectInterpreter(x hom (new (KeyValueStore ~> KeyValueStoreEffect) {
      def apply[X](c: KeyValueStore[X]) =
        KeyValueStoreNoEffect(KeyValueStoreInterpreter(More(c map (Done(_)))))
    }))

  def put[A](k: String, v: String): KeyValueStoreEffectInterpreter[Option[String]] =
    lift(KeyValueStoreInterpreter.put(k, v))

  def get[A](k: String): KeyValueStoreEffectInterpreter[Option[String]] =
    lift(KeyValueStoreInterpreter.get(k))

  def del[A](k: String): KeyValueStoreEffectInterpreter[Option[String]] =
    lift(KeyValueStoreInterpreter.del(k))

  // Interpreter instruction to print (out) the given string.
  def println[A](s: String): KeyValueStoreEffectInterpreter[Unit] =
    KeyValueStoreEffectInterpreter(More(KeyValueStoreEffectPrintln(s, Done(()))))

  // Interpreter instruction to print (out) the given string.
  def putPrintln[A](k: String, v: String): KeyValueStoreEffectInterpreter[Unit] =
    KeyValueStoreEffectInterpreter(More(KeyValueStoreEffectPutPrintln(k, v, Done(()))))

}
