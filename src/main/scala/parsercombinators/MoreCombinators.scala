package parsercombinators

/**
  * Created by ariwaranosai on 15/12/8.
  *
  */

trait MoreCombinators extends SimpleParsers {
    def success[T](v: T): Parser[T]
        = Parser { in => Success(v, in)(Failure("unknown failure", in))}

    def opt[T](p: => Parser[T]): Parser[Option[T]]
         = (p ^^ {x: T => Some(x)}) | success(None)

    def rep[T](p: => Parser[T]): Parser[List[T]]
        = rep1(p) | success(List())

    def rep1[T](p: => Parser[T]): Parser[List[T]]
        = rep1(p, p)

    def rep1[T](first: => Parser[T], other: => Parser[T]): Parser[List[T]]
        = first ~ rep(other) ^^ mkList

    def repsep[T, S](p: => Parser[T], q: => Parser[S]): Parser[List[T]]
        = rep1seq(p, q) | success(List())

    def rep1seq[T, S](p: Parser[T], q: Parser[S]): Parser[List[T]]
        = rep1seq(p, p, q)

    def rep1seq[T, S](p: Parser[T], p1: Parser[T], q: Parser[S]): Parser[scala.List[T]]
        = p ~ rep(q ~> p) ^^ mkList

    def chainl1[T](p: Parser[T], q: => Parser[(T, T) => T]): Parser[T]
        = chainl1(p, p, q)

    def chainl1[T, U](first: Parser[T], p: Parser[U], q: Parser[(T, U) => T]): Parser[T]
        = first ~ rep(q ~ p) ^^ {
        case (x, xs) => xs.foldLeft(x) {(_, _) match { case (a, (f, b)) => f(a, b) }}
    }

    implicit def acceptSeq[ES](es: ES)(implicit ev1: ES => Iterable[Elem]): Parser[List[Elem]] = {
        def acceptRec(pxs: Parser[List[Elem]], x: Elem) = (accept(x) ~ pxs) ^^ mkList
        es.foldRight[Parser[List[Elem]]](success(Nil))((x, y) => acceptRec(y, x))
    }

    private def mkList[T] = (y: (T, List[T])) => y match {
        case (x, xs) => x::xs
    }
}

object OXOParserNew extends StringParsers with MoreCombinators {
    def oxy = "oxy".log("oxy") ^^ { x => x.mkString("")}
    def oxys = rep1seq(oxy, ' ').log("oxy")

    def main(args: Array[String]) = println((oxys ~ eoi.log("eoi"))("oxy oxy oxy"))
}

object DeclareParser extends StringParsers with MoreCombinators {

    def member =  "val".log("val") ~! "he".log("he")| "def".log("def") ~! "fuck".log("fuck")

    def main(args: Array[String]) = println((member.log("member") ~ eoi.log("eoi"))("valfe"))
}

object ArithmeticParsers extends StringParsers with MoreCombinators { self =>

    def digit = '1' or '2' or '3' or '4' or '5' or '6' or '7' or '8' or '9'

    def number = rep(digit) ^^ {x => x.mkString("").toInt}

    def plus = '+' ^^ {_ => (x: Int, z: Int) => x + z}
    def mins = '-' ^^ {_ => (x: Int, z: Int) => x - z}
    def prod = '*' ^^ {_ => (x: Int, z: Int) => x * z}
    def div = '/' ^^ {_ => (x: Int, z: Int) => x / z}

    def expr = chainl1(term, plus or mins)
    def term = chainl1(factor, prod or div)
    def factor: Parser[Int] = '(' ~> expr <~ ')' or number

    def main(args: Array[String]) = println(expr("(13+12)/(5*1)"))
}


