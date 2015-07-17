package rdbisch.datastructures

sealed trait MyList[+A]
case object Nil extends MyList[Nothing]
case class Cons[+A](head: A, tail: MyList[A]) extends MyList[A]

object MyList {
  def apply[A](as: A*): MyList[A] = 
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}
