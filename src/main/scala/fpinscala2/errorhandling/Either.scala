package fpinscala2.errorhandling

import scala.util.control.NonFatal
import fpinscala2.errorhandling.Main.map2Both

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

  def map2Both[E, A, B, C](a: Either[E,A], b: Either[E, B], f: (A, B) => C): Either[List[E], C] = {
    (a, b) match {
      case (Right(aa), Right(bb)) => Right(f(aa, bb))
      case (Left(e), Right(_))    => Left(List(e))
      case (Right(_), Left(e))    => Left(List(e))
      case (Left(e1), Left(e2))   => Left(List(e1, e2))
    }
  }

  def map2All[E, A, B, C](a: Either[List[E], A], b: Either[List[E], B], f: (A,B) => C): Either[List[E], C] = {
    (a, b) match {
      case (Right(aa), Right(bb)) => Right(f(aa, bb))
      case (Left(es1), Left(es2)) => Left(es1 ++ es2)
      case (Left(es), Right(_))   => Left(es)
      case (Right(_), Left(es))   => Left(es)
    }
  }

  def traverseAll[E, A, B](as: List[A], f: A => Either[List[E], B]): Either[List[E], List[B]] = {
    as.foldRight(Right(Nil): Either[List[E], List[B]])((a, acc) => map2All(f(a), acc, _ :: _))
  }

  def sequenceAll[E, A](as: List[Either[List[E], A]]): Either[List[E], List[A]] = {
    traverseAll(as, identity)
  }
}

case class Name private (value: String)
object Name {
  import fpinscala2.errorhandling.Either._
  def apply(name: String): Either[String, Name] = {
    if (name == "" | name == null){
      Left("Name is empty.")
    }else{
      Right(new Name(name))
    }
  }
}

case class Age private (value: Int)
object Age {
  import fpinscala2.errorhandling.Either._
  def apply(age: Int): Either[String, Age] = {
    if (age < 0){
      Left("Age is out of range.")
    }else {
      Right(new Age(age))
    }
  }
}

case class Person(name: Name, age: Age)
object Person{
  def makeBoth(name: String, age: Int): Either[List[String], Person] = {
    map2Both(Name(name), Age(age), Person(_,_))
  }
  def make(name: String, age: Int): Either[String, Person] = {
    Name(name).map2(Age(age))(Person(_,_))
  }
}
