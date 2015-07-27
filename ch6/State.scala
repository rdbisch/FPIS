case class State[S, +A](run: S => (A, S)) {
    def map[B](f: A => B): State[S, B] = State(
        state => {
            val (aval, state2) = run(state)
            (f(aval), state2)
        }
    )

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = State(
        state => {
            val (aval, state2) = run(state)
            val (bval, state3) = sb.run(state2)
            (f(aval, bval), state3)
        }
    )

    def flatMap[B](sa: A => State[S, B]): State[S, B] = State(
        state => {
            val (a, state2) = run(state)
            sa(a).run(state2)
        }
    )
}

object State {
    def unit[S, A](a: A) : State[S, A] = State(state => (a, state))
    def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
        fs.foldRight(unit[S, List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))

}

def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
} yield ()

def get[S]: State[S, S] = State(s => (s, s))
def set[S](s: S): State[S, Unit] = State(_ => ((), s))

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int) {
    def update(input:Input) : Machine = { 
        input match {
            case _ if candies == 0 => this
            case Coin => Machine(false, candies, coins + 1)
            case Turn if locked => this
            case Turn => Machine(true, candies - 1, coins)
        }
    }

    def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = inputs match {
        case el :: els => update(el).simulateMachine(els)
        case Nil => State(_ => ((candies, coins), this))
    }
}


