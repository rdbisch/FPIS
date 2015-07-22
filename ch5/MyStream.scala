import MyStream._
trait MyStream[+A] {

    def headOption: Option[A] = this match {
        case Empty => None
        case Cons(h, t) => Some(h())
    }

    def toList : List[A] = this match {
        case Cons(h, t) => h() :: t().toList
        case Empty => Nil
    }

    def take(n : Int): MyStream[A] = this match {
        case Cons(h, t) if n > 0 => cons(h(), t().take(n-1))
        case _ => Empty
    }

    def drop(n: Int): MyStream[A] = this match {
        case Cons(h, t) if n > 0 => t().drop(n - 1)
        case _ => this
    }

    def takeWhile(p: A => Boolean): MyStream[A] = {
        this match {
            case Cons(h, t) if p(h()) => cons(h(), t() takeWhile p)
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
        foldRight(Empty : MyStream[A])((a, b) => if (p(a)) cons(a, b) else empty)
    }

    def headOption2 : Option[A] = {
        foldRight(None:Option[A])( (a, b) => Some(a) )
    }

    def map[B]( f: A => B) : MyStream[B] = this match {
        case Cons(h, t) => cons(f(h()), t().map(f)) 
        case Empty => empty
    }

    def map2[B]( f: A => B ) : MyStream[B] =
        foldRight(empty[B])((a, b) => cons(f(a), b))

    def filter( f: A => Boolean ) : MyStream[A] = 
        foldRight(empty[A])((a, b) => if (f(a)) cons(a, b) else b)

    def append[B>:A]( s: MyStream[B] ) : MyStream[B] = 
        foldRight(this)((a, b) => cons(a, b))

    def flatMap[B]( f: A => MyStream[B] ) : MyStream[B] = 
        foldRight(empty[B])((a, b) => f(a) append b)

    def unfold[B, S](z: S)(f: S => Option[(B, S)]) : MyStream[B] = this match {
        case Cons(h, t) => 
            f(z) match =>
                Some((b: B, s: S)) => cons(z, unfold(s)(f))
                None => empty
    
        case Empty => empty
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


    def constant[A](a: A): MyStream[A] = {
        cons(a, constant(a))
    }

    def from(n: Int): MyStream[Int] = {
        cons(n, from(n + 1))
    }

    def fib() : MyStream[Int] = fib(1,1)
    def fib(a: Int, b: Int) : MyStream[Int] = cons(b, fib(b, a + b))


}

