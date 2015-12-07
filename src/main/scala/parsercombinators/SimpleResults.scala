package parsercombinators

/**
  * Created by ariwaranosai on 15/12/7.
  */

trait SimpleResults {
    type Input

    trait Result[+T] {
        def next: Input
    }

    case class Success[+T](result: T, next: Input) extends Result[T]
    case class Failure(msg: String, next: Input) extends Result[Nothing]
}

object XParser extends SimpleResults {
    type Input = String

    val acceptX: Input  => Result[Char] = {
        (in: String) => if (in.length > 0 && in.charAt(0) == 'x')
            Success('x', in.substring(1))
            else Failure("expected an x", in)
    }
}

class CharParser(x: Char) extends SimpleResults {
    type Input = String

    val acceptX: Input => Result[Char] = {
        (in: String) => if (in.length > 0 && in.charAt(0) == x)
            Success(x, in.substring(1))
            else Failure("expected an " + x.toString, in)
    }
}
