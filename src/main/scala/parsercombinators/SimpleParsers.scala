package parsercombinators

/**
  * Created by ariwaranosai on 15/12/7.
  */

trait SimpleParsers extends SimpleResults {
    trait Parser[+T] extends (Input => Result[T]) { self =>
        def apply(in: Input): Result[T]

        def | [U >: T](p: => Parser[U]): Parser[U] = new Parser[U] {
            def apply(in: Input) = Parser.this(in) match {
                case s@Success(x, n) => s
                case Failure(_, _) => p(in)
            }
        }

        def ~ [U](p: => Parser[U]): Parser[(T, U)] = new Parser[(T, U)] {
            def apply(in: Input) = Parser.this(in) match {
                case Success(x, next) => p(next) match {
                    case Success(x2, next2) => Success((x, x2), next2)
                    case f@Failure(m, n) => f
                }
                case f@Failure(m, n) => f
            }
        }

        def log(name: String) = new Parser[T] {
            def apply(in: Input): Result[T] = {
                println("trying " + name + " at \'" + in + "\'")
                val r = self(in)
                println(name + " -> " + r)
                r
            }
        }
    }


}

trait StringParsers extends SimpleParsers {
    type Input = String
    private val EOI = 0.toChar

    def accept(expected: Char) = new Parser[Char] {
        def apply(in: String) =
            if (in == "") {
                if (expected == EOI)
                    Success(expected, "")
                else
                    Failure("no more input", in)
            } else if(in.charAt(0) == expected) {
                Success(expected, in.substring(1))
            } else {
                Failure("expected \'" + expected + "\'", in)
            }
    }

    def eoi = accept(EOI)
}

object OXOParser extends StringParsers {
    def oxo = accept('o') ~ accept('x') ~ accept('o')
    def oxos: Parser[Any] = (oxo ~ accept(' ') ~ oxos) | oxo

    def main(args: Array[String]) = println((oxos.log("oxos") ~ eoi.log("eoi"))(args(0)))
}