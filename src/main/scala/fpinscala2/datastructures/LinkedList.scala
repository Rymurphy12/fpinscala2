package fpinscala2.datastructures

enum List[+A] {
  case Nil
  case Cons(head: A, tail: List[A])
}


object List {

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail*))
  }

  @main def main(): Unit = {
    // Exercise 3.1
    var tstLyst = List(1,2,3,4,5)
    val result = tstLyst match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y // <- My Choice for Answer 3
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }
    println(result)
    println(tail(tstLyst))
    println(drop(tstLyst, 6)) // Nil
    println(drop(tstLyst, 1)) // List(2,3,4,5)
    println(drop(tstLyst, 5)) // Nil
    println(dropWhile(tstLyst, x => x < 3))
    println(dropWhile(tstLyst, x => x < 0))
    println(dropWhile(tstLyst, x => x < 6))
  }

  // Exercise 3.2
  def tail[A](xs: List[A]): List[A] = {
    xs match {
      case Nil => sys.error("You can't get the tail of an empty List!")
      case Cons(_, tail) => tail
    }
  }

  // Exercise 3.3
  def setHead[A](xs: List[A], x: A): List[A] = {
    xs match {
      case Nil => sys.error("You can't replace the head of an empty List!")
      case Cons(_, tail) => Cons(x, tail)
    }
  }

  // Exercise 3.4
  def drop[A](as: List[A], n: Int): List[A]  = {
    (as, n) match {
      case (Nil, _) => Nil
      case (lyst, num) if(num <= 0) => lyst
      case (Cons(head, tail), num) => drop(tail, num - 1)
    }
  }

  // Exercise 3.5
  def dropWhile[A](as: List[A], f: A => Boolean): List[A] = {
    as match {
      case Nil => Nil
      case Cons(head, tail) if(f(head)) => dropWhile(tail, f)
      case _ => as
    }
  }

  // Exercise 3.6
  def init[A](as: List[A]): List[A] = {
    as match {
      case Cons(x, Cons(_, Nil)) => Cons(x, Nil)
      case Nil => sys.error("Can't init an empty list")
      case Cons(head, tail) => Cons(head, init(tail))
    }
    // Answer: it can't be constant time because a Linked List is head biased
    // This requires us to go through each connected list until we find the last
    // on to discard
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = {
    a1 match {
      case Nil => a2
      case Cons(head, tail) => Cons(head, append(tail, a2))
    }
  }

  def sum(ints: List[Int]): Int = {
    ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }
  }

  def product(doubles: List[Double]): Double = {
    doubles match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
    }
  }
}
