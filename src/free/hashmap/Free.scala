package free
package hashmap

private case class Done[F[+_], +A](a: A) extends Free[F, A]
private case class More[F[+_], +A](a: F[Free[F, A]]) extends Free[F, A]
// a.k.a. codensity hack. Scala does not have proper TCO.
private case class Bind[F[+_], A, +B](x: () => Free[F, A], f: A => Free[F, B]) extends Free[F, B]

sealed trait Free[F[+_], +A] {
  def map[X](f: A => X)(implicit F: Functor[F]): Free[F, X] =
    flatMap(a => Done(f(a)))

  def flatMap[X](f: A => Free[F, X])(implicit F: Functor[F]): Free[F, X] =
    this match {
      case Bind(x, g) =>
        Bind(x, (x: Any) => Bind(() => g(x), f))
      case _ =>
        Bind(() => this, f)
    }
    /*
    // flatMap proper: no codensity hack
    this match {
      case Done(a) => f(a)
      case More(k) => More(F.fmap((_: Free[F, A]) flatMap f)(k))
    }
    */

  @annotation.tailrec
  final def resume(implicit F: Functor[F]): Resume[F, A] =
    this match {
      case Done(a) =>
        Term(a)
      case More(a) =>
        Cont(a)
      case Bind(x, f) =>
        x() match {
          case Done(a) =>
            f(a).resume
          case More(a) =>
            Cont(F.fmap((_: Free[F, Any]) flatMap f)(a))
          case Bind(y, g) =>
            y().flatMap((x: Any) => g(x) flatMap f).resume
        }
    }

  def maps[G[+_]](f: F ~> G)(implicit F: Functor[F], G: Functor[G]): Free[G, A] =
    resume match {
      case Cont(x) =>
        More(f(F.fmap((_: Free[F, A]) maps f)(x)))
      case Term(a) =>
        Done(a)
    }

  def mapf(f: F ~> F)(implicit F: Functor[F]): Free[F, A] =
    resume match {
      case Cont(x) =>
        More(f(x))
      case Term(a) =>
        Done(a)
    }

  final def go[AA >: A](f: F[Free[F, AA]] => Free[F, AA])(implicit F: Functor[F]): AA = {
    @annotation.tailrec def go2(t: Free[F, AA]): AA = t.resume match {
      case Cont(x) => go2(f(x))
      case Term(a) => a
    }
    go2(this)
  }

}
