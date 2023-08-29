package fpinscala2.datastructures

enum Tree[+A]{
	case Leaf(value: A)
	case Branch(left: Tree[A], right: Tree[A])

	def size: Int = {
		this.match {
			case Leaf(_) => 1
			case Branch(l, r) => 1 + l.size + r.size
		}
	}

	// Exercise 3.26
	def depth: Int = {
		this match {
			case Leaf(_) => 0
			case Branch(l, r) => l.depth.max(r.depth)
		}
	}

	// Exercise 3.27
	def map[B](f: A => B): Tree[B] = {
		this match {
			case Leaf(value) => Leaf(f(value))
			case Branch(left, right) => Branch(left.map(f), right.map(f))
		}
	}

	// Exercise 3.28
	def fold[B](f: A => B, g: (B, B) => B): B = {
		this match {
			case Leaf(value) => f(value)
			case Branch(left, right) => g(left.fold(f,g), right.fold(f,g))
		}
	}
	// 3.28 Implementations
	def depthByFold: Int = {
		this.fold(x => 0, (l,r) => l.max(r))
	}

	def mapByFold[B](f: A => B): Tree[B] = {
		this.fold(x => Leaf(f(x)), (l, r) => Branch(l, r))
	}

	def sizeByFold: Int = {
		this.fold(x => 1, (l, r) => l + r + 1)
	}
}

object Tree {
	extension (t: Tree[Int]) def firstPositive: Int = {
		t match {
			case Leaf(i) => i
			case Branch(l, r) => {
				val lpos = l.firstPositive
				if lpos > 0 then lpos else r.firstPositive
			}
		}
	}

	// Exercise 3.25
	extension (t: Tree[Int]) def maximum: Int = {
		t match {
			case Leaf(i) => i
			case Branch(l,r) => l.maximum.max(r.maximum)
		}
	}

	// Exercise 3.28 Implementation
	extension (t: Tree[Int]) def maximumByFold: Int = {
		t.fold(x => x, (x, y) => x.max(y))
	}

}