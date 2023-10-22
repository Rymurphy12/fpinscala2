package fpinscala2.laziness

enum LazyList[+A]{
	import LazyList._
	case Empty
	case Cons(h: () => A, t: () => LazyList[A])

	def headOption: Option[A] = {
		this match {
			case Empty => None
			case Cons(h, t) => Some(h())
		}
	}

	// Exercise 5.1
	def toList: List[A] = {
		@annotation.tailrec
		def go(lzLyst: LazyList[A], acc: List[A]): List[A] = {
			lzLyst match {
				case Cons(h, t) => go(t(), h() :: acc)
				case Empty => acc.reverse
			}
		}
		go(this, Nil)
	}

	// Exerise 5.2
	def take(n: Int): LazyList[A] = {
		this match
			case Cons(h, t) if n > 0 => cons(h(), t().take(n - 1))
			case _  => empty
	}

	// Exerise 5.2
	def drop(n: Int): LazyList[A] = {
		this match {
			case Cons(h,t) if n > 0 => t().drop(n - 1)
			case _ => empty
		}
	}

	// Exercise 5.3
	def takeWhile(p: A => Boolean): LazyList[A] = {
		this match {
			case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
			case _ => empty
		}
	}
}


object LazyList{
	def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] = {
		lazy val head = hd
		lazy val tail = tl
		Cons(() => head, () => tail)
	}

	def empty[A]: LazyList[A] = Empty

	def apply[A](as: A*): LazyList[A] = {
		if as.isEmpty then empty
		else cons(as.head, apply(as.tail*))
	}
}
