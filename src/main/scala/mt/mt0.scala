package mt

import scalaz._
import Scalaz._
import effect._, IO._

/**
  * Created by ariwaranosai on 16/5/11.
  *
  */

sealed abstract class LoginError
case object InvalidEmail extends LoginError {
    override def toString = "InvalidEmail"
}
case object NoSuchUser extends LoginError {
    override def toString = "NoSuchUser"
}
case object WrongPassword extends LoginError {
    override def toString = "WrongPassword"
}


object mt0 {
    import EitherIO._

    def getDomain(email: String): \/[LoginError, String] =
        email.split('@') match {
            case x if x.length == 2 => x(1).right
            case _ => InvalidEmail.left
        }

    val users = Map("example.com"->"qwer", "localhost"->"1234")

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

    def getToken0: IO[\/[LoginError, String]] =
        for {
            _ <- IO.putStrLn("Enter email address:")
            email <- IO.readLn
        } yield getDomain(email)

    def getToken1: EitherIO[LoginError, String] =
        for {
            _ <- EitherIO(IO.putStrLn("Enter email address:").map(_.right[LoginError]))
            email <- EitherIO(IO.readLn.map(_.right[LoginError]))
            m <- EitherIO(IO(getDomain(email)))
        } yield m

    import EitherIO._

    def getToken: EitherIO[LoginError, String] =
        for {
            _ <- liftIO[LoginError, Unit](putStrLn("Enter email address:"))
            email <- liftIO[LoginError, String](readLn)
            m <- liftEither[LoginError, String](getDomain(email))
        } yield m

//            EitherIO(IO.putStrLn("Enter email address:").map(_.right[LoginError])) >>= {
//                _ => EitherIO(IO.readLn.map(_.right[LoginError])) >>= {
//                    email => EitherIO(IO(getDomain(email)))
//                }
//            }

    def userLogin0: IO[\/[LoginError, String]] =
        for {
            token <- getToken0
            x <- token match {
                case \/-(domain) => {
                    users.get(domain) match {
                        case Some(userpw) => for {
                            _ <- putStrLn("Enter password")
                            passwd <- readLn
                        } yield {
                            if (userpw == passwd) token else WrongPassword.left
                        }
                        case None => IO(NoSuchUser.left)
                    }}
                case left => IO(left)
            }
        } yield x

//
//    def userLogin: IO[\/[LoginError, String]] =
//        for {
//            token <- getToken
//            userpw <- maybe(liftEither[LoginError, String](InvalidEmail.left))(
//                (x: String) => liftEither(x.right)
//            )(
//                users.get(token)
//            )
//            passwd <- liftIO[LoginError, String](putStrLn("Enter your password:") >> readLn)
//            x <- {if (userpw == passwd) token.point[EitherIO[LoginError, String]] else liftEither(-\/(WrongPassword))}
//        } x

    def userLogin1: EitherIO[LoginError, String] =
        for {
            token <- getToken
            userpw <- users.get(token).fold(
                liftEither[LoginError, String](InvalidEmail.left)
            )(
                (x: String) => EitherIO[LoginError, String](IO(x.right))
            )
            passwd <- liftIO[LoginError, String](putStrLn("Enter your password:") >> readLn)
            m <- if(passwd != userpw)
                    liftEither[LoginError, String](-\/(WrongPassword))
                 else
                    liftEither[LoginError, String](token.right)
        } yield m

    def printResultM(eio: LoginError \/ String): IO[Unit] = {
        val t = eio match {
            case \/-(token) => "login in with token: " + token
            case -\/(InvalidEmail)  => "Invalid email address entered."
            case -\/(NoSuchUser) => "No user with that email exists."
            case  -\/(WrongPassword) => "Wrong password."
        }

        putStrLn(t)
    }

    def main(args: Array[String]): Unit = {
        (userLogin1.runEitherIO >>= printResultM).unsafePerformIO()
    }
}

