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

  /**  Exercise 2.2 */
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(as: Array[A]): Boolean = {
      if (as.length <= 1) true
      else ordered(as(0), as(1)) && go(as.slice(1, as.length))
    }
    go(as)
  }

  private def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

  private def testIsSorted(as: Array[Int]) : String = {
    def comp(a: Int, b: Int) : Boolean = a < b
    val msg = "The array %s is sorted: %b."
    msg.format(as.mkString(","), isSorted(as, comp))
  }

  /** Exercise 2.3 */
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
    (a: A) => (b: B) => f(a, b)
  }

  /** Exercise 2.4 */
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  def testCurry() = {
    val test = curry( (x:Int, y:Int) => x + y )
    val test2 = test(4)
    println(test2(5))
    val test3 = uncurry(test)
    println(test3(4,5))
  }

  /** Exercise 2.5 */
  def compose[A,B,C](f: B=>C, g: A=>B): A=>C = {
    (a: A) => f(g(a))
  }

  def testCompose() {
    val doubleFactorial = compose( factorial, factorial )
    println(doubleFactorial(3))
  }

  def main(args: Array[String]): Unit =
    println(formatResult("absolute value", -42, abs))
    println(formatResult("factorial", 7, factorial))
    println(formatResult("fibanocci", 10, fib))
    println(testIsSorted(Array(1,2,3,4,5)))
    println(testIsSorted(Array(1,2,5,4,5)))
    println(testIsSorted(Array()))
    println(testIsSorted(Array(1)))
    println(testIsSorted(Array(1,2)))
    println(testIsSorted(Array(2,1)))
    println(testIsSorted(Array(1,2,5,4,5,9,1,23,2,24,3,4,5,1,2,3,4,5)))
    testCurry()
    testCompose()
}
