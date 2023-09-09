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