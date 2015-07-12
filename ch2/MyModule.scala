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

  private def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

  def main(args: Array[String]): Unit =
    println(formatResult("absolute value", -42, abs))
    println(formatResult("factorial", 7, factorial))
    println(formatResult("fibanocci", 10, fib))
}
