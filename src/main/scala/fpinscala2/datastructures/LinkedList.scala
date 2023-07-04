package fpinscala2.datastructures

enum List[+A] {
  case Nil
  case Cons(head: A, tail: List[A])
}


object List{
  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail*))
  }
}