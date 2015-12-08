package parsercombinators

/**
  * Created by ariwaranosai on 15/12/7.
  */

trait SimpleParsers extends SimpleResults {
    def Parser[T](f: Input => Result[T])
        = new Parser[T] { def apply(in: Input) = f(in)}
    /*
     * Parser 是一个类型,接受 Input 返回一个结果,为了是combinator的,加入了高阶函数.
     */
    trait Parser[+T] extends (Input => Result[T]) { self =>
        def apply(in: Input): Result[T]

        def flatMap[U](f: T => Parser[U]): Parser[U]
            = Parser { in => Parser.this(in).flatMapWithNext(f)}

        def map[U](f: T => U): Parser[U] = Parser { in => Parser.this(in).map(f)}

        def | [U >: T](p: => Parser[U]): Parser[U] = Parser { in => Parser.this(in).append(p(in)) }

        def ~ [U](p: => Parser[U]): Parser[(T, U)] =  for {
            x <- this
            y <- p
        } yield (x, y)

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
    def oxos: Parser[List[String]] = ((oxo ~ accept(' ')).map(_ => "oxo") ~ oxos).map(x => x._1 :: x._2) | oxo.map(_ => List("oxo"))

    def main(args: Array[String]) = println((oxos.log("oxos") ~ eoi.log("eoi"))(args(0)))
}