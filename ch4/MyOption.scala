sealed trait MyOption[+A]
case class Some[+A](get: A) extends MyOption[A]
case object None extends MyOption[Nothing]

object MyOption {
    def map[A,B](opt: MyOption[A])(f: A => B): MyOption[B] = opt match {
        case None    => None
        case Some(a) => Some(f(a))
    }

    def 
}
