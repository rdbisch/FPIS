sealed trait MyTree[+A]
case class Leaf[A](value: A) extends MyTree[A]
case class Branch[A](left: MyTree[A], right: MyTree[A]) extends MyTree[A]

object MyTree {

    /* Exercise 3.25 */
    def countNodes[A]( tree: MyTree[A] ) : Int = tree match {
        case Leaf(a) => 1
        case Branch(left, right) => countNodes(left) + countNodes(right)
    }

    /* Exercise 3.26 */
    def maxElement( tree: MyTree[Int] ) : Int = tree match {
        case Leaf(a) => a
        case Branch(left, right) => maxElement(left) max maxElement(right)
    }

    /* Exercise 3.27 */
    def depth[A]( tree: MyTree[A] ) : Int = tree match {
        case Leaf(a) => 0
        case Branch(left, right) => 1 + (depth(left) max depth(right))
    }

    /* Exercise 3.28 */
    def map[A, B]( tree: MyTree[A] )( f: A => B ) : MyTree[B] = tree match {
        case Leaf(a) => Leaf(f(a))
        case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }

    /* Exercise 3.29 */
   def fold[A, B]( tree: MyTree[A] )( f: A => B )(g: (B, B) => B): B = tree match {
       case Leaf(a) => f(a)
       case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
   }
   def countNodes2[A]( tree: MyTree[A] ) : Int = fold(tree)((x:A) => 1)(_ + _)
   def maxEl2( tree: MyTree[Int] ) : Int = fold(tree)((x:Int) => x)((x:Int, y:Int) => x max y)
   def map2[A, B](tree: MyTree[A])( f: A=>B ): MyTree[B] =
       fold(tree)(a => Leaf(f(a)) : MyTree[B])(Branch(_,_))
}

