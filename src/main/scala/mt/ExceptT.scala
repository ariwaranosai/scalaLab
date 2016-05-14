package mt
import scalaz._
import Scalaz._
import scalaz.effect.IO
import scalaz.effect.IO._

/**
  * Created by sai on 2016/5/12.
  */

final case class ExceptT[E, M[_], A](runExceptT: M[\/[E, A]])

object ExceptT {
  implicit def monadInstance[E, M[_]](implicit func: Monad[M]) = new Monad[({type l[A] = ExceptT[E, M, A]})#l] with Functor[({type l[A] = ExceptT[E, M, A]})#l]{
    override def point[A](a: => A) = ExceptT[E, M, A](func.point(\/-(a)))

    override def map[A, B](et: ExceptT[E, M, A])(f: A => B): ExceptT[E, M, B] = {
      val fmapped = func.map(et.runExceptT)(x => x.map(f))
      ExceptT(fmapped)
    }
    override def bind[A, B](et: ExceptT[E, M, A])(f: A => ExceptT[E, M, B]): ExceptT[E, M, B] =
      ExceptT(
        et.runExceptT.flatMap(_.fold(
          (l: E) => func.point(l.left),
          (r: A) => f(r).runExceptT
        ))
      )
  }


  def liftEither[E, M[_], A](et: \/[E, A])(implicit m: Monad[M]): ExceptT[E, M, A] = ExceptT(m.point(et))
  def lift[E, M[_], A](et: M[A])(implicit m: Monad[M]): ExceptT[E, M, A] = ExceptT(m.map(et)(_.right[E]))
  def throwE[E, M[_], A](e: E)(implicit m: Monad[M]): ExceptT[E, M, A] = liftEither(e.left[A])
  def catchE[E, M[_], A, C](throwing: => ExceptT[E, M, A])(handler: E => ExceptT[C, M, A])(implicit m: Monad[M]): ExceptT[C, M, A] =
    ExceptT(for {
      x <- throwing.runExceptT
      r <- (x: \/[E, A]) match {
        case \/-(success) => m.point(success.right[C])
        case -\/(failure) => handler(failure).runExceptT
      }
    } yield r)
}

//private trait ExceptTFunctor[F[_], E] extends Functor[({type l[A] = ExceptT[E, F, A]})#l] {
//  implicit def F: Functor[F]
//  override def map[A, B](fa: ExceptT[E, F, A])(f: A => B): ExceptT[E, F, B] = fa map f
//}
//
//private trait ExceptTBind[F[_], E] extends Monad[({type l[A] = ExceptT[E, F, A]})#l]  with ExceptTFunctor[F, E] {
//  implicit def M: Monad[F]
//  override def bind[A, B](fa: ExceptT[E, F, A])(f: A => ExceptT[E, F, B]): ExceptT[E, F, B] = fa flatMap f
//  override def point[A](a: => A): ExceptT[E, F, A] =  ExceptT(M.point(\/-(a)))
//}
//
//private abstract class EitherInstances {
//  implicit def eitherTFunctor[F[_], E](implicit F0: Functor[F]) = new ExceptTFunctor[F, E] {
//    implicit def F = F0
//  }
//
//}

object mt {
  import ExceptT._
  val users = Map("example.com"->"qwer", "localhost"->"1234")

  def getDomain(email: String): \/[LoginError, String] =
    email.split('@') match {
      case x if x.length == 2 => x(1).right
      case _ => InvalidEmail.left
    }


  def printResult0(domain: \/[LoginError, String]): IO[Unit] =
    domain match {
      case \/-(text) => putStrLn("Domain: " + text)
      case -\/(InvalidEmail) => putStrLn("Error: Invalid domain")
    }

  def either[A, B, C](f: A => C)(g: B => C)(e: \/[A, B]): C =
    e match {
      case \/-(r) => g(r)
      case -\/(l) => f(l)
    }

  def maybe[A, B](f: B)(g: A => B)(e: Option[A]): B =
    e match {
      case Some(x) => g(x)
      case None => f
    }

  def printResult(domain: \/[LoginError, String]): IO[Unit] =
    domain.fold(
      y => IO.putStrLn(y.toString),
      x => IO.putStrLn("Domain: " + x)
    )

  def getToken: ExceptT[LoginError, IO, String] =
    for {
      _ <- lift[LoginError, IO, Unit](IO.putStrLn("Enter email address:"))
      input <- lift[LoginError, IO, String](IO.readLn)
      m <- liftEither[LoginError, IO, String](getDomain(input))
    } yield m

  def userLogin: ExceptT[LoginError, IO, String] =
    for {
      token <- getToken
      userpw <- maybe(throwE[LoginError, IO, String](NoSuchUser))(
        (x: String) => ExceptT[LoginError, IO, String](IO(x.right)))(users.get(token))
      passwd <- lift[LoginError, IO, String](IO.putStrLn("Enter your password:") >> IO.readLn)
      m <- if(passwd != userpw)
        liftEither[LoginError, IO, String](-\/(WrongPassword))
      else
        liftEither[LoginError, IO, String](token.right)
    } yield m

  def wrongPasswordHandler(er: LoginError): ExceptT[LoginError, IO, String] =
    er match {
      case WrongPassword => for {
        _ <- lift[LoginError, IO, Unit](putStrLn("Wrong password, one more chance"))
        x <- userLogin
      } yield x

      case err => throwE(err)
    }

  def printError[A](err: LoginError): ExceptT[LoginError, IO, A] =
    for {
      _ <- lift[LoginError, IO, Unit] {
        putStrLn {
          err match {
            case WrongPassword => "Wrong password. No more changes."
            case NoSuchUser => "No user with that email exists."
            case InvalidEmail => "Invalid email address entered."
          }
        }
      }
      x <- throwE[LoginError, IO, A](err)
    } yield x

  def loginDialogue : ExceptT[LoginError, IO, Unit] =
    for {
      token <- {
        val retry = catchE(userLogin)(wrongPasswordHandler)
        catchE(retry)(printError)
      }
      x <- lift[LoginError, IO, Unit](putStrLn("Logged in with token: " + token))
    } yield x


  def main(args: Array[String]): Unit = {
    // (userLogin.runExceptT >>= printResult).unsafePerformIO()
    // type EEither[E, A] = ExceptT[E, Identity, A]
    loginDialogue.runExceptT.unsafePerformIO()
  }
}
