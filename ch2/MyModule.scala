// A comment!
/* Another comment */
/** A documentation comment */

object MyModule {
  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n - 1, n*acc)

    go(n, 1)
  }

  /** Exercise 2.1 **/
  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(a: Int, b: Int, n: Int): Int = {
      if (n <= 0) a;
      else go(b, a + b, n - 1);
    }

    go(0, 1, n)
  }

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d."
    msg.format(x, abs(x))
  }

  private def formatFactorial(n: Int) = {
    val msg ="The factorial of %d is %d."
    msg.format(n, factorial(n))
  }

  private def formatFibanocci(n: Int) = {
    val msg ="The %d(th) fib number is %d."
    msg.format(n, fib(n))
  }

  def main(args: Array[String]): Unit =
    println(formatAbs(-42))
    println(formatFactorial(7))
    println(formatFibanocci(10))
}
