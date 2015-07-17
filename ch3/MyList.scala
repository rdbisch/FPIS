package rdbisch.datastructures

sealed trait MyList[+A]
case object Nil extends MyList[Nothing]
case class Cons[+A](head: A, tail: MyList[A]) extends MyList[A]

object MyList {
  def apply[A](as: A*): MyList[A] = 
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  /* Exercise 3.2 */
  def tail[A](el: MyList[A]): MyList[A] = el match {
    case Nil => Nil
    case Cons(x, xs) => xs
  }

  /* Exercise 3.3 */
  def setHead[A](el: MyList[A], a: A): MyList[A] = el match {
    case Nil => Nil
    case Cons(x, xs) => Cons(a, xs)
  }

  /* Exercise 3.4 */
  def drop[A](el: MyList[A], n: Int): MyList[A] = n match {
    case 0 => el
    case _ => el match {
      case Nil => Nil
      case Cons(x, xs) => drop(xs, n - 1)
    }
  }

  /* Exercise 3.5 */
  def dropWhile[A](el: MyList[A])(filter: A => Boolean): MyList[A] = el match {
    case Cons(x, xs) if filter(x) => dropWhile(xs)(filter)
    case _ => el
  }

  /* Exercise 3.6 */
  def init[A](el: MyList[A]) : MyList[A] = el match {
    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def foldRight[A, B](as: MyList[A], z: B)(f: (A, B) => B): B = 
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  /* Exercise 3.9 */
  def length[A](as: MyList[A]): Int = foldRight(as, 0)((x, y) => 1 + y)

  /* Exercise 3.10 */
  @annotation.tailrec
  def foldLeft[A, B](as: MyList[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  }

}
