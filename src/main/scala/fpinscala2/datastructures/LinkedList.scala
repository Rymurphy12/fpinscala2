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

    // Exercise 3.8
    // Prediction: This will return a List[List[Int]]
    var foldResult = foldRight(List(1,2,3), Nil: List[Int], (x, y) => Cons(x, y))
    println(foldResult)
    // Result : I was wrong List[Int]. For some reason I was thinking the entire list
    //          would be placed in the head position.
    // Analysis: Fold Right can properly determine the resultig type by looking at the values
    //           in both the collection and the accumulator

    println(length(Nil))
    println(length(List(1)))
    println(length(List(1,2,3)))
    println(reverse(List(1,2,3)))
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

  // Exercise 3.7
  // Answer: At least how foldRight is currently implemented there is no good way to do it.
  // You could possible do a matching statement and return Nil if 0.0, however this will still
  // return the final accumulated value and not zero like it should.

  // Exercise 3.9
  def length[A](as: List[A]): Int = {
    foldRight(as, 0, (_, y) => y +1)
  }

  // Exercise 3.10
  @annotation.tailrec
  def foldLeft[A, B](as: List[A], acc: B, f: (B, A) => B): B = {
    as match {
      case Cons(x, y) => foldLeft(y, f(acc, x), f)
      case Nil => acc
    }
  }

  // Exercise 3.11 Part 1
  def sumViaFoldLeft(ns: List[Int]): Int = {
    foldLeft(ns, 0, _+_)
  }

  // Exercise 3.11 Part 2
  def productViaFoldLeft(ds: List[Double]): Double = {
    foldLeft(ds, 1.0, _*_)
  }

  // Exercise 3.11 Part 3
  def lengthViaFoldLeft[A](as: List[A]): Int = {
    foldLeft(as, 0, (acc, _) => acc + 1)
  }

  // Exercise 3.12
  def reverse[A](as: List[A]): List[A] = {
    foldLeft(as, Nil: List[A], (tl, hd) => Cons(hd, tl))
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

  def foldRight[A,B](as: List[A], acc: B, f: (A, B) => B): B = {
    as match {
      case Nil  => acc
      case Cons(x, xs) => f(x, foldRight(xs, acc, f))
    }
  }

  def sumViaFoldRight(ns: List[Int]): Int = {
    foldRight(ns, 0, (x,y) => x + y)
  }

  def productViaFoldRight(ns: List[Double]): Double = {
    foldRight(ns, 1.0, _*_)
  }

}
