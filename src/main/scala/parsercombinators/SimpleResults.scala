package parsercombinators

/**
  * Created by ariwaranosai on 15/12/7.
  */

trait SimpleResults {
    type Input
    type Elem
    def first(in: Input): Elem
    def rest(in: Input): Input

    /*
     * 对每次的结果抽象出来一个类型,这个类型最初只包含接下来的内容(这个很直观).
     * 然后为了结果是可combinator的,加入了更多的函数
     */
    trait Result[+T] {
        def next: Input
        def map[U](f: T => U): Result[U]
        def flatMapWithNext[U](f: T => Input => Result[U]): Result[U]
        def append[U >: T](alt: => Result[U]): Result[U]
        def flatMap[U](f: T => Result[U]): Result[U]
        def filter(f: T => Boolean): Result[T] = this
    }

    case class Success[+T](result: T, next: Input)(val zero: Failure) extends Result[T] {
        def map[U](f: T => U) = Success(f(result), next)(zero)
        def flatMapWithNext[U](f: T => Input => Result[U]) = f(result)(next)
        def append[U >: T](alt: => Result[U]) = this
        def flatMap[U](f: T => Result[U]): Result[U] = f(result)
        override def filter(f: T => Boolean): Result[T] = if (f(result)) this else zero
    }

    case class Failure(msg: String, next: Input) extends Result[Nothing] {
        def map[U](f: Nothing => U) = this
        def flatMapWithNext[U](f: Nothing => Input => Result[U]) = this
        def append[U](alt: => Result[U]) = alt
        def flatMap[U](f: Nothing => Result[U]): Result[U] = this
    }
}

object XParser extends SimpleResults {
    type Input = String
    type Elem = Char

    def first(in: Input): Elem = if(in == "") 0.toChar else in(0)
    def rest(in: Input): Input = if(in == "") in else in.substring(1)

    val acceptX: Input  => Result[Char] = {
        (in: String) => if (in.length > 0 && in.charAt(0) == 'x')
            Success('x', in.substring(1))(Failure("unknow error", in))
            else Failure("expected an x", in)
    }
}

class CharParser(x: Char) extends SimpleResults {
    type Input = String
    type Elem = Char

    def first(in: Input): Elem = if(in == "") 0.toChar else in(0)
    def rest(in: Input): Input = if(in == "") in else in.substring(1)

    val acceptX: Input => Result[Char] = {
        (in: String) => if (in.length > 0 && in.charAt(0) == x)
            Success(x, in.substring(1))(Failure("unknow error", in))
            else Failure("expected an " + x.toString, in)
    }
}
