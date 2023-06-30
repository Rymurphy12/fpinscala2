package CH2

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
    }

    def factorial(n: Int): Int = {
        @annotation.tailrec
        def go(n: Int, acc: Int): Int = {
            if (n <= 0) acc
            else  go(n - 1, n * acc)
        }
        go(n, 1)
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
}
