package mt

import scalaz._
import Scalaz._
import effect._, IO._

/**
  * Created by ariwaranosai on 16/5/11.
  *
  */

case class EitherIO[E, A](runEitherIO: IO[\/[E,A]])

object EitherIO {
    implicit def eitherIOFunctorInstance[E] = new Functor[({type l[X]  = EitherIO[E, X]})#l] with Monad[({type l[X] = EitherIO[E, X]})#l]{
        override def point[A](a: => A) = EitherIO[E, A](IO(\/-(a)))

        override def map[A, B](em: EitherIO[E, A])(f: A => B): EitherIO[E, B]  = {
            val unwarpped = em.runEitherIO
            val fmapped = unwarpped.map(x => x.map(f))
            EitherIO(fmapped)
        }

        override def bind[A, B](em: EitherIO[E, A])(f: A => EitherIO[E, B]): EitherIO[E, B] =
            EitherIO(em.runEitherIO.flatMap(_.fold(
                (l:E) => IO(l.left),
                (r:A) => f(r).runEitherIO
            )))

    }

}