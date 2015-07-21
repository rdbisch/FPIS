
sealed trait MyStream[+A] {

    def headOption: Option[A] = this match {
        case Empty => None
        case Cons(h, t) => Some(h())
    }

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
            case Cons(h, t) if p(h()) => Cons(h, () => t().takeWhile(p))
            case _ => Empty
        }
    }

    def foldRight[B](z: => B)(f: (A, =>B) => B): B = this match {
        case Cons(h, t) => f(h(), t().foldRight(z)(f))
        case _ => z
    }

    def exists(p: A => Boolean): Boolean =
        foldRight(false)((a, b) => p(a) || b)

    def forAll(p: A => Boolean): Boolean =
        foldRight(true)((a, b) => p(a) && b)

    def takeWhile2(p: A => Boolean): MyStream[A] = {
        foldRight(Empty : MyStream[A])((a, b) => if (p(a)) Cons(() => a, () => b) else Empty)
    }

    def headOption2 : Option[A] = {
        foldRight(None:Option[A])( (a, b) => Some(a) )
    }

    def map[B]( f: A => B) : MyStream[B] = this match {
        case Cons(h, t) => Cons(() => f(h()), () => t().map(f))
        case Empty => Empty
    }

    def map2[B]( f: A => B ) : MyStream[B] =
        foldRight(Empty:MyStream[B])((a, b) => Cons( () => f(a), () => b ))

    def filter( f: A => Boolean ) : MyStream[A] = 
        foldRight(Empty:MyStream[A])((a, b) => if (f(a)) Cons(() => a, () => b.filter(f)) else b.filter(f))

    def flatMap[B]( f: A => MyStream[B] ) : MyStream[B] = 
        foldRight(Empty:MyStream[B])((a, b) => MyStream.appendStream(f(a), b))

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

    def append[A]( stream : MyStream[A], item : A ) : MyStream[A] = 
        stream.foldRight( MyStream(item) )( (a, b) => Cons( () => a, () => b ))

    def appendStream[A]( stream: MyStream[A], other: MyStream[A]) : MyStream[A] = 
        other.foldRight( stream )( (a, b) => MyStream.append(b, a) )
}

