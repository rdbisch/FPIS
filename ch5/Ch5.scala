
sealed trait MyStream[+A] {
    def toList : List[A] = this match {
        case Cons(h, t) => h() :: t().toList
        case Empty => Nil
    }

    def take(n : Int): MyStream[A] = this match {
        case Cons(h, t) if n > 0 => Cons(h, () => t().take(n-1))
        case _ => Empty
    }

    def drop(n: Int): MyStream[A] = this match {
        case Cons(h, t) if n > 0 => t().drop(n - 1)
        case _ => this
    }

    def takeWhile(p: A => Boolean): MyStream[A] = {
        this match {
            case Cons(h, t) => {
                val he = h()
                if (p(he)) Cons( () => he, t )
                else Empty
            }
            case _ => Empty
        }
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

