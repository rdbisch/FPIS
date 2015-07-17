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

  /* Exercise 3.12 */
  def revList[A](as: MyList[A]) : MyList[A] = {
    foldLeft(as, Nil:MyList[A])((x, y) => Cons(y, x))
  }

  /* Exercise 3.13 */
  def foldRight2[A, B](as: MyList[A], z: B)(f: (A, B) => B): B = {
    foldLeft(revList(as), z)( (b:B, a:A) => f(a, b) )
  }

  /* Exercise 3.14 */
  def append[A](as: MyList[A], item: A) : MyList[A] = {
    foldRight2(as, Cons(item, Nil))((x, y) => Cons(x, y))
  }

  /* Exercise 3.14 */
  def appendList[A](as: MyList[A], bs: MyList[A]) : MyList[A] = {
    foldLeft(as, bs)(append)
  }

  /* Exercise 3.15 */
  def appendLists[A](as: MyList[MyList[A]]) : MyList[A] = {
    foldLeft(as, Nil:MyList[A])(appendList)
  }

  /* Exercise 3.16 . oops also 3.18 */
  def map[A, B](as: MyList[A])( f: A => B ) : MyList[B] = {
    foldRight(as, Nil:MyList[B])( (y, x) => Cons(f(y), x) )
  }
  /* Exercise 3.16 */
  def addOne(as: MyList[Int]) = map(as)(_ + 1)
  /* Exercise 3.17 */
  def dblToString(as: MyList[Double]) = map(as)(_.toString)

  /* Exercise 3.19 */
  def filter[A](as: MyList[A])(f: A => Boolean): MyList[A] = {
    foldRight2(as, Nil: MyList[A])( (x, y) => f(x) match {
        case true => Cons(x, y)
        case false => y
      })
  }

  /* Exercise 3.20 */
  def flatMap[A, B](as: MyList[A])( f: A => MyList[B] ): MyList[B] = {
    appendLists(
      foldLeft(as, Nil:MyList[MyList[B]])( (x, y) => Cons(f(y), x) )
    )
  }
}
