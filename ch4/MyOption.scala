trait MyOption[+A] {
  def map[B](f: A => B): MyOption[B] = this match {
    case None    => None
    case Some(a) => Some(f(a))
  }

  def getOrElse[B >: A](default : => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def flatMap[B](f: A => MyOption[B]): MyOption[B] =
    map(f) getOrElse None

  def filter(f: A => Boolean): MyOption[A] = 
    this match {
      case Some(a) if f(a) => this
      case _ => None
    }
}
case class Some[+A](get: A) extends MyOption[A]
case object None extends MyOption[Nothing]

