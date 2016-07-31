package learningscalaz

/**
  * Created by ariwaranosai on 16/6/17.
  *
  */

import scalaz._
import Scalaz._


object T {
    sealed trait Toy[+A, +Next]
    case class Output[A, Next](a: A, next: Next) extends Toy[A, Next]
    case class Bell[Next](next: Next) extends Toy[Nothing, Next]
    case class Done() extends Toy[Nothing, Nothing]
}

//case class Fix[F[_]](f: F[Fix[F]])
//object Fix {
//    def fix[A](toy: Toy[A, Fix[Toy[A, ?]]]) = Fix[Toy[A, ?]](toy)
//}
//import Fix._
//fix[Char](Output('A', fix[Char](Done())))


//sealed trait FixE[F[_], E]
//object FixE {
//    case class Fix[F[_], E](f: F[FixE[F ,E]]) extends FixE[F, E]
//    case class Throwy[F[_], E](e: E) extends FixE[F, E]
//
//    def fix[A, E](toy: Toy[A, FixE[Toy[A, ?], E]]): FixE[Toy[A, ?], E]
//        = Fix[Toy[A, ?], E](toy)
//
//    def throwy[F[_], E](e: E): FixE[F, E] = Throwy(e)
//
//    def catchy[F[_]: Functor, E1, E2](ex: => FixE[F, E1])
//        (f: E1 => FixE[F, E2]): FixE[F, E2] = ex match {
//        case Fix(x) => Fix[F, E2](Functor[F].map(x) { catchy(_)(f)})
//        case Throwy(e) => f(e)
//    }
//}


//case class IncompleteException()
//def subroutine = FixE.fix[Char, IncompleteException](
//    Output('A',
//        FixE.throwy[Toy[Char, ?], IncompleteException](IncompleteException())))
//
//def program = FixE.catchy[Toy[Char, ?], IncompleteException, Nothing](subroutine) { _ =>
//    FixE.fix[Char, Nothing](Bell(FixE.fix[Char, Nothing](Done())))
//}


//
//def output[A](a: A): Free[Toy[A, ?], Unit] =
//    Free.roll[Toy[A, ?], Unit](Output(a, Free.pure[Toy[A, ?], Unit](())))
//
//def bell[A]: Free[Toy[A, ?], Unit] =
//    Free.roll(Bell(Free.point[Toy[A, ?], Unit](())))
//
//def done[A]: Free[Toy[A, ?], Unit] = Free.roll[Toy[A, ?], Unit](Done())

object Toy {
    import T._
    implicit def ToyFunctor[Y]: Functor[Toy[Y, ?]] = new Functor[Toy[Y, ?]] {
        override def map[A, B](fa: Toy[Y, A])(f: (A) => B): Toy[Y, B] =
            fa match {
                case o: Output[Y, A] => Output(o.a, f(o.next))
                case b: Bell[A] => Bell(f(b.next))
                case Done() => Done()
            }
    }

    def output[A](a: A): Free[Toy[A, ?], Unit] =
        Free.liftF[Toy[A, ?], Unit](Output(a, ()))

    def bell[A]: Free[Toy[A, ?], Unit] =
        Free.liftF[Toy[A, ?], Unit](Bell())

    def done[A]: Free[Toy[A, ?], Unit] = Free.liftF[Toy[A, ?], Unit](Done())

}

    object main {
        import T._

        def showProgram[R: Show, A: Show](p: Free[Toy[A, ?], R]): String =
            p.resume.fold({
                case Output(a, next) =>
                    "output " + Show[A].shows(a) + "\n" + showProgram(next)
                case Bell(next) =>
                    "bell " + "\n" + showProgram(next)
                case Done() =>
                    "done\n"
            }, { r: R => "return" + Show[R].shows(r) + "\n" })

        import Toy._

        val subroutine = output('A')

        val program = for {
            _ <- subroutine
            _ <- bell
            _ <- done
        } yield ()

        def pretty[R: Show, A: Show](p: Free[Toy[A, ?], R]) = print(showProgram(p))

        pretty(program)

    }