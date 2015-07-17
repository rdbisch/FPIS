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
}
