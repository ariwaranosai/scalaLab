package parsercombinators

import scala.language.implicitConversions
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

        def filter(f: T => Boolean): Parser[T] = Parser {
            in => this(in) filter f
        }


        def log(name: String) = Parser { in => {
                println("trying " + name + " at \'" + in + "\'")
                val r = self(in)
                println(name + " -> " + r)
                r
            }
        }
    }

    def consumeFirst: Parser[Elem] = Parser { in =>
        Success(first(in), rest(in))(Failure("unknow failure", in))
    }

    def acceptIf(p: Elem => Boolean): Parser[Elem] = consumeFirst filter p

    implicit def accept(e: Elem): Parser[Elem] = acceptIf(_ == e)

}

trait StringParsers extends SimpleParsers {
    type Input = String
    type Elem = Char
    private val EOI = 0.toChar

    def first(in: Input): Elem = if(in == "") EOI else in(0)
    def rest(in: Input): Input = if(in == "") in else in.substring(1)

    def eoi = accept(EOI)
}


object OXOParser extends StringParsers {
    def oxo = 'o' ~ 'x' ~ 'o'
    def oxos: Parser[List[String]] = ((oxo ~ ' ').map(_ => "oxo") ~ oxos).map(x => x._1 :: x._2) | oxo.map(_ => List("oxo"))

    def main(args: Array[String]) = println((oxos.log("oxos") ~ eoi.log("eoi"))(args(0)))
}