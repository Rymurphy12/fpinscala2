package fpinscala2.errorhandling

import fpinscala2.errorhandling.Either._

enum Validated[+E, +A] {
  case Valid(get: A)
  case Invalid(errors: E)

  def toEither: Either[E, A] = {
    this match {
      case Valid(a) => Right(a)
      case Invalid(errors) => Left(errors)
    }
  }

  def map[B](f: A => B): Validated[E, B] = {
    this match {
      case Valid(a) => Valid(f(a))
      case Invalid(es) => Invalid(es)
    }
  }

  def map2[EE >: E, B, C](b: Validated[EE, B])(f: (A, B) => C)(combinedErrors: (EE, EE) => EE): Validated[EE, C] = {
    (this, b) match {
      case (Valid(aa), Valid(bb))  => Valid(f(aa, bb))
      case (Invalid(es), Valid(_)) => Invalid(es)
      case (Valid(_), Invalid(es)) => Invalid(es)
      case (Invalid(es1), Invalid(es2)) => Invalid(combinedErrors(es1, es2))
    }
  }
}

object Validated {
  def fromEither[E, A](e: Either[E, A]): Validated[E, A] = {
    e match {
      case Right(value) => Valid(value)
      case Left(value) => Invalid(value)
    }
  }

  def traverse[E, A, B](as: List[A], f: A => Validated[E, B], combinedErrors: (E, E) => E): Validated[E, List[B]] = {
    as.foldRight(Valid(Nil): Validated[E, List[B]])((a, acc) => f(a).map2(acc)(_::_)(combinedErrors))
  }

  def sequence[E, A](vs: List[Validated[E, A]], combinedErrors: (E, E) => E): Validated[E, List[A]] = {
    traverse(vs, identity, combinedErrors)
  }
}
