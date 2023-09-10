package fpinscala2.errorhandling

enum Option[+A]{
	case Some(get: A)
	case None

	// Exercise 4.1 Pt1
	def map[B](f: A => B): Option[B] = {
		this match {
			case Some(value) => Some(f(value))
			case None => None
		}
	}
	
	// Exercise 4.1 Pt2
	def flatMap[B](f: A => Option[B]): Option[B] ={
		this.map(x => f(x)).getOrElse(None)
	}

	// Exercise 4.1 Pt3
	def getOrElse[B >: A](default: => B): B = {
		this match {
			case Some(get) => get
			case None => default
		}
	}

	// Exercise 4.1 Pt4
	def orElse[B >: A](ob: => Option[B]): Option[B] ={
		this.map(x => Some(x)).getOrElse(ob)
	}

	// Exercise 4.1 Pt5 Note: I implemented this in terms of flatMap rather than
	// map and/or getOrElse. Note sure if that is fully correct
	def filter(f: A => Boolean): Option[A] = {
		this.flatMap(x => if(f(x)) Some(x) else None)
	}
	
	def mean(xs: Seq[Double]): Option[Double] = {
		if (xs.isEmpty) None else Some(xs.sum / xs.length)
	}

	//Exercise 4.2
	def variance(xs: Seq[Double]): Option[Double] = {
		// Not sure if this one is correct
		// Update: According to the book it is correct
		mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
	}

}

object Option {

	// Exercise 4.3
	def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
		(a,b) match {
			case (_, None) => None
			case (None, _) => None
			case (Some(x), Some(y)) => Some(f(x, y))
		}
		// Alt solution in book
		// a.flatMap(aa => b.map(bb => f(aa, bb)))
	}

	// Exercise 4.4
	def sequence[A](as: List[Option[A]]): Option[List[A]] = {
		// Ehh....I have my doubts. This is either not right or
		// unnecessarily complex. Note: I used Scala's List class
		// not the one I got from the book
		as.foldRight(Some(Nil: List[A]))((aa, acc) => map2(aa, acc)(_ :: _))
		// Update: I almost got it correct. For the z paramater I used
		// None: Option[List[A]]. The correct answer is Some(Nil: List[A])
		// I am assuming this would cause it to be None every time
	}

	// Exercise 4.5
	def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = {
		as.foldRight(Some(Nil: List[B]))((aa, bb) =>  map2(f(aa), bb)(_ :: _))
	}

	def sequenceViaTraverse[A](as: List[Option[A]]): Option[List[A]] = {
		traverse(as)(x => x)
	}
}