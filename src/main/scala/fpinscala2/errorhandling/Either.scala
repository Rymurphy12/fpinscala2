package fpinscala2.errorhandling

import scala.util.control.NonFatal

enum Either[+E, +A] {
  case Left(value: E)
  case Right(value: A)

  // Exercise 4.6
  def map[B](f : A => B): Either[E, B] ={
    this match {
      case Left(e) => Left(e)
      case Right(a) => Right(f(a))
    }
  }
  
  // Exercise 4.6
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
    this match {
      case Left(e) => Left(e)
      case Right(a) => f(a)
    }
  }

  // Exercise 4.6
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = {
    this match {
      case Right(a) => Right(a)
      case _ => b
    }
  }

  // Exercise 4.6
  def map2[EE >: E, B, C](that: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    this.flatMap(ts => that.map(ta => f(ts, ta)))
  }
}

object Main {
  import Either.{Left, Right}
  def mean(xs: Seq[Double]): Either[String, Double] = {
    if (xs.isEmpty)
      Left("Mean of empty List")
    else
      Right(xs.sum / xs.length)
  }

  def safeDiv(x: Int, y: Int):Either[Throwable, Int] = {
    try Right(x /y)
    catch case NonFatal(t) => Left(t)
  }

  def catchNonFatal[A](a: => A): Either[Throwable, A] = {
    try Right(a)
    catch case NonFatal(t) => Left(t)
  }

  // Exercise 4.7
  def sequence[E, A](as: List[Either[E, A]]): Either[E, List[A]] = {
    as.foldRight(Right(Nil): Either[E, List[A]])((x,acc) => x.map2(acc)(_::_))
  }

  // Exerise 4.7
  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    as.foldRight(Right(Nil): Either[E, List[B]])((aa, bb) => f(aa).map2(bb)(_::_))
  }
}
