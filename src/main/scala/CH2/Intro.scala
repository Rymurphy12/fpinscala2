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

    @main def printAbs(): Unit = {
        println(formatAbs(-42))
    }
}
