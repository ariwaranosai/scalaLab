/**
  * Created by ariwaranosai on 16/6/17.
  *
  */
import scalaz._

object SS {

    sealed trait Trampoline[+A] {
        final def runT: A =
            this match {
                case More(k) => k().runT
                case Done(v) => v
            }
    }

    case class More[+A](k: () => Trampoline[A]) extends Trampoline[A]

    case class Done[+A](result: A) extends Trampoline[A]

}


object FM {
    type Pair[A] = (A, A)
    type BinTree[A] = Free[Pair, A]
    type Tree[A] = Free[List, A]
    type FreeMonoid[A] = Free[(A, ?), Unit]
    type Trival[A] = Unit
    type Option[A] = Free[Trival, A]

    type Trampoline[A] = Free[Function0, A]
}