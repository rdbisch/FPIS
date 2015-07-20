
sealed trait MyStream[+A] {
    def toList : List[A] = this match {
        case Cons(h, t) => h() :: t().toList
        case Empty => Nil
    }
}
case object Empty extends MyStream[Nothing]
case class Cons[+A](h: () => A, t: () => MyStream[A]) extends MyStream[A]

object MyStream {
    def cons[A](hd: => A, tl: => MyStream[A]): MyStream[A] = {
        lazy val head = hd
        lazy val tail = tl
        Cons(() => head, () => tail)
    }
    def empty[A]: MyStream[A] = Empty

    def apply[A](as: A*): MyStream[A] = 
        if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

}

