package free
package hashmap

case class Cont[F[+_], +A](x: F[Free[F, A]]) extends Resume[F, A]
case class Term[F[+_], +A](x: A) extends Resume[F, A]

sealed trait Resume[F[+_], +A] {
  def map[B](f: A => B)(implicit F: Functor[F]): Resume[F, B] =
    this match {
      case Cont(x) =>
        Cont(F.fmap((_: Free[F, A]) map f)(x))
      case Term(a) =>
        Term(f(a))
    }

  def free: Free[F, A] =
    this match {
      case Cont(x) =>
        More(x)
      case Term(a) =>
        Done(a)
    }

  def term: Option[A] =
    this match {
      case Cont(_) =>
        None
      case Term(a) =>
        Some(a)
    }

  def termOr[AA >: A](a: => AA): AA =
    term getOrElse a

  def cont: Option[F[Free[F, A]]] =
    this match {
      case Cont(x) =>
        Some(x)
      case Term(a) =>
        None
    }

  def contOr[AA >: A](x: => F[Free[F, AA]]): F[Free[F, AA]] =
    cont getOrElse x

}
