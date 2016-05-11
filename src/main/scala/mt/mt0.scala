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


object mt {
    def getDomain(email: String): \/[LoginError, String] =
        email.split('@') match {
            case x if x.length == 2 => x(1).right
            case _ => InvalidEmail.left
        }

    // todo
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

    def getToken: EitherIO[LoginError, String] =
        for {
            _ <- EitherIO(IO.putStrLn("Enter email address:").map(_.right[LoginError]))
            email <- EitherIO(IO.readLn.map(_.right[LoginError]))
            m <- EitherIO(IO(getDomain(email)))
        } yield m

//            EitherIO(IO.putStrLn("Enter email address:").map(_.right[LoginError])) >>= {
//                _ => EitherIO(IO.readLn.map(_.right[LoginError])) >>= {
//                    email => EitherIO(IO(getDomain(email)))
//                }
//            }

    def userLogin: IO[\/[LoginError, String]] =
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

    def main(args: Array[String]): Unit = {
        getToken.runEitherIO.map(println(_)).unsafePerformIO()
    }
}

