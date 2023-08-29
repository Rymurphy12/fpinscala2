package fpinscala2.datastructures

import scala.compiletime.ops.boolean

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
    val lystInts = List(1,2,3)
    val lystIntsTwo = List(4,5,6)
    val lystDouble = List(1.0, 2.0, 3.0)
    val longerLyst = List(1,2,3,4,5,6,7,8)
    val smallLystOne = List(1,2)
    val smallLystTwo = List(4,5)
    println(length(Nil))
    println(length(List(1)))
    println(length(lystInts))
    println(reverse(lystInts))
    println(appendViaFoldLeft(lystInts, List(4,5,6)))
    println(appendViaFoldRight(lystInts, List(4,5,6)))
    val lystOfLysts = List(lystInts, List(4,5,6), List(7,8))
    println(concatListOfList(lystOfLysts))
    println(addOneList(lystInts))
    println(listDoubleToListString(lystDouble).isInstanceOf[List[String]])
    println(map(lystInts, x => x + 1))
    println(filter(longerLyst, _ < 5))
    println(filterViaFlatMap(longerLyst, _ < 5))
    println(flatMap(lystInts, i => List(i, i)))
    println(addTwoLists(lystInts, lystIntsTwo))
    println(addTwoLists(lystInts, smallLystTwo))
    println(addTwoLists(smallLystOne, lystIntsTwo))
    println(hasSubsequence(lystInts, smallLystOne))
    println(hasSubsequence(lystInts, List(3)))
    println(hasSubsequence(lystInts, List(2,3,4)))
    println(hasSubsequence(lystInts, Nil))
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

  // Exercise 3.13
  def foldRightViaFoldLeft[A,B](as: List[A], acc: B, f: (A, B) => B): B = {
    // I Haven't confirmed the answer on this, but I seem to remember discovering 
    // it was deceptively easy the first time arround
    // I don't beleive we can implement foldleft by foldright as the case statements
    // make adjust the sturcture impossible.
    foldLeft(reverse(as), acc, (x, y) => f(y, x))
    // Answer - I was half-correct/half-wrong. My answer for foldRightViaFoldLeft
    // is correct but the second half is wrong. It can be done but the acc needs
    // to be an identity function and the f needs to be a function that returns
    // a function taht returns the answer. Truthfully, I would not have figure that out.
  }

  // Exercise 3.14 Pt1
  def appendViaFoldLeft[A](a1: List[A], a2: List[A]): List[A] = {
    foldLeft(reverse(a1), a2, (x, y) =>  Cons(y, x))
  }

  // Exercise 3.14 Pt2
  def appendViaFoldRight[A](a1: List[A], a2: List[A]): List[A] = {
    foldRightViaFoldLeft(a1, a2, (x, y) => Cons(x, y))
  }

  // Exercise 3.15
  def concatListOfList[A](llxs: List[List[A]]): List[A] = {
    foldRightViaFoldLeft(llxs, Nil: List[A], (x, y) => appendViaFoldRight(x, y))
  }

  // Exercise 3.16
  def addOneList(ns: List[Int]): List[Int] = {
    foldRightViaFoldLeft(ns, Nil: List[Int], (h, t) => Cons(h +1, t))
  }

  // Exercise 3.17
  def listDoubleToListString(ds: List[Double]): List[String] = {
    foldRightViaFoldLeft(ds, Nil: List[String], (h, t) => Cons(h.toString(), t))
  }

  // Exercise 3.18
  def map[A, B](as: List[A], f: A => B): List[B] = {
    foldRightViaFoldLeft(as, Nil: List[B], (x, y) => Cons(f(x), y))
  }

  // Exercise 3.19
  def filter[A](as: List[A], f: A => Boolean): List[A] = {
    @annotation.tailrec
    def loop(as: List[A], acc: List[A]): List[A] = {
        as match {
          case Cons(h, t) if f(h) => loop(t, Cons(h, acc))
          case Cons(_, t) => loop(t, acc)
          case Nil => acc
        }
    }
    loop(reverse(as), Nil: List[A])
  }

  // Exercise 3.20
  def flatMap[A, B](as: List[A], f: A => List[B]): List[B] = {
    foldRightViaFoldLeft(as, Nil: List[B], (x, acc) => appendViaFoldRight(f(x), acc))
  }

  // Exercise 3.21
  def filterViaFlatMap[A](as: List[A], f: A => Boolean): List[A] = {
    flatMap(as, x => x match {
      case x if(f(x)) => List(x)
      case _ => Nil
    })
  }

  // Exercise 3.22
  def addTwoLists(xs: List[Int], ys: List[Int]): List[Int] = {
    @annotation.tailrec
    def loop(as: List[Int], bs: List[Int], acc: List[Int]): List[Int] = {
      (as, bs) match {
        // Not sure about this behavior. I assumed if there if one list is longer
        // we do not include it in the list as there is nothing to add it to
        //case (Cons(h1, t1), Cons(h2, t2)) => loop(t1, t2, appendViaFoldRight(acc, List(h1 + h2))) // My Initial Answer
        case (Cons(h1, t1), Cons(h2, t2)) => loop(t1, t2, Cons(h1 + h2, acc)) // Better Answer
        case _ => acc
      }
    }
    reverse(loop(xs, ys, Nil: List[Int]))
  }

  // Exercise 3.23
  def zipWith[A, B](xs: List[A], ys: List[A], f: (A, A) => B): List[B] = {
    @annotation.tailrec
    def loop(as: List[A], bs: List[A], acc: List[B]): List[B] = {
      (as, bs) match {
        // Made similar assumptions as in 3.22
        //case (Cons(h1, t1), Cons(h2, t2)) => loop(t1, t2, appendViaFoldRight(acc, List(f(h1, h2)))) // My Initial Answer
        case (Cons(h1, t1), Cons(h2, t2)) => loop(t1, t2, Cons(f(h1, h2), acc)) // Better answer
        case _ => acc
      }
    }
    reverse(loop(xs, ys, Nil: List[B]))
  }

  // Exercise 3.24
  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    @annotation.tailrec
    def hasMatch(checkedList: List[A], subToCheck: List[A]): Boolean = {
      (checkedList, subToCheck) match {
        case (Nil, Cons(_, _)) => false
        case (_, Nil) => true
        case (Cons(h1, t1), Cons(h2, t2)) => if(h1 != h2) false else hasMatch(t1, t2)
      }
    }
    sup match {
      case Nil => false
      case Cons(_, t) => if(hasMatch(sup, sub)) true else hasSubsequence(t, sub)
    }
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
