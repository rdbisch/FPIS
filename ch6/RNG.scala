trait RNG {
  def nextInt: (Int, RNG)

  def nonNegativeInt: (Int, RNG) = {
    val (int, rng2) = nextInt
    if (int < 0) (-(int + 1), rng2)
    else (int, rng2)
  }

  def double: (Double, RNG) = {
    val (int, rng2) = nonNegativeInt
    (int.toDouble / Int.MaxValue, rng2)
  }

  def intDouble: ((Int, Double), RNG) = {
    val (int, rng2) = nextInt
    val (dbl, rng3) = rng2.double
    ((int, dbl), rng3)
  }

  def doubleInt: ((Double, Int), RNG) = {
    val ((x, y), rng2) = intDouble
    ((y, x), rng2)
  }

  def double3: ((Double, Double, Double), RNG) = {
    val (dbl, rng2) = double
    val (dbl2, rng3) = rng2.double
    val (dbl3, rng4) = rng3.double
    ((dbl, dbl2, dbl3), rng4)
  }
}

object RNG {
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = count match {
    case x if x < 0 => (Nil, rng)
    case _ =>
      val (el, rng2) = ints(count - 1)(rng)
      val (int, rng3) = rng2.nextInt
      (int :: el, rng3)
    
  }
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

type Rand[+A] = RNG => (A, RNG)
val int: Rand[Int] = _.nextInt
def unit[A](a: A): Rand[A] = rng => (a, rng)

/*           (s: RNG => (A, RNG))(f: A=> B): (RNG => (B, RNG)) */
def map[A, B](s: Rand[A])(f: A => B): Rand[B] = 
  rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

val double2 = map(int)(i => i.toDouble / Int.MaxValue)

def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
  rng => {
    val (a, rng2) = ra(rng)
    val (b, rng3) = rb(rng2)
    (f(a,b), rng3)
  }
}

def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = 
  map2(ra, rb)((_, _))

/* Exercise 6.7 -- Oops I made it too hard */
def sequenceOfStateTransactions[A, B](count: Int, ra: Rand[A])(f: A => B): Rand[List[B]] = 
  rng => {
    count match {
      case x if x < 0 => (Nil, rng)
      case _ =>
        val (el, rng2) = sequenceOfStateTransactions(count - 1, ra)(f)(rng)
        val (int, rng3) = ra(rng2)
        (f(int) :: el, rng3)
      }
    }

/* Exercise 6.7 -- Simpler version */
def sequence2[A](count: Int, ra: Rand[A]) : Rand[List[A]] = 
  rng => {
    count match {
      case x if x < 0 => (Nil, rng)
      case _ =>
        val (el, rng2) = sequence2(count - 1, ra)(rng)
        val (int, rng3) = ra(rng2)
        (int :: el, rng3)
      }
    }

/* Exercise 6.8 */
def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = 
  rng => {
    val (a, rng2) = f(rng)
    g(a)(rng2)
  }

def mapB[A, B](s: Rand[A])(f: A => B): Rand[B] = 
  /* Rand[B]  :   RNG => (B, RNG)  */
  flatMap(s)( a => unit(f(a)) )

