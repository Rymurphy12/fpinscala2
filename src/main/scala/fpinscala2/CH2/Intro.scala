package fpinscala2.CH2

// A comment !
/* Another Comment */
/** A documentation comment */
object MyProgram{
    def abs(n: Int): Int = {
        if n < 0 then -n
        else n
    }

    private def formatAbs(x: Int) = {
        val msg = "The absolute value of %d is %d"
        msg.format(x, abs(x))
    }

    private def formatFactorial(n: Int) = {
        val msg = "The factoral of %d is %d"
        msg.format(n, factorial(n))
    }

    def formatResult(name: String, n: Int, f: Int => Int): String = {
        val msg = "The %s of %d is %d"
        msg.format(name, n, f(n))
    }

    @main def printAbsAndFactorial(): Unit = {
        println(formatResult("absolute value", -42, abs))
        println(formatResult("factorial", 7, factorial))
        println(fib(7))
        println(findFirst(Array(7, 9, 13), x => x == 9))
        println(isSorted(Array(1,2,3), _ > _))
        println(isSorted(Array(1, 2, 1), _ > _))
        println(isSorted(Array(3, 2, 1), _ < _))
        println(isSorted(Array(1, 2, 3), _ < _))
        println(isSorted(Array(1), _ > _))
        println(isSorted(Array(1, 2), _ < _))
        println(isSorted(Array(1, 2), _ > _))
    }

    def factorial(n: Int): Int = {
        @annotation.tailrec
        def go(n: Int, acc: Int): Int = {
            if (n <= 0) acc
            else  go(n - 1, n * acc)
        }
        go(n, 1)
    }

    def findFirst[A](as: Array[A], p: A => Boolean): Int = {
        @annotation.tailrec
        def loop(n: Int): Int = {
            if (n >= as.length) -1
            else if(p(as(n))) then n
            else loop(n + 1)
        }
        loop(0)
    }

    // Exercise 2.1
    def fib(n: Int): Int = {
        @annotation.tailrec
        def loop(n: Int, first: Int, second: Int): Int = {
            if (n <= 0) first
            else loop(n-1, second, first + second)
        }
        loop(n ,0,1)
    }

    // Exercise 2.2
    def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
        @annotation.tailrec
        def loop(n: Int): Boolean = {
            if (n + 1 >= as.length) true
            else if (gt(as(n), as(n + 1))) then false
            else loop(n +1)

        }
        loop(0)
    }

    // Exercise 2.3
    def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
        (a: A) => (b : B) => f(a, b)
    }

    // Exercise 2.4
    def uncurry[A,B,C](f : A => B => C): (A, B) => C = {
        (a: A, b: B) => f(a)(b)
    }

    // Exercise 2.5
    def compose[A, B, C](f: B => C, g: A => B): A => C = {
        (a: A) => f(g(a))
    }
}
