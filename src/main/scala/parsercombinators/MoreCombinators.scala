package parsercombinators

import scala.util.matching.Regex

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


trait MoreStringParsers extends StringParsers with MoreCombinators {
    implicit def regex(r: Regex): Parser[String] = Parser { in =>
        val msg = "expect regex " + r
        r.findPrefixOf(in) match {
            case None => Failure(msg, in)
            case Some(m) =>
                Success(m, in.substring(m.length))(Failure("unknown error", in))
        }
    }

    val empty = Parser[String] { in  => Success("", in)(Failure("unknown error", in))}
}

object OXOParserNew extends StringParsers with MoreCombinators {
    def oxy = "oxy".log("oxy") ^^ { x => x.mkString("")}
    def oxys = rep1seq(oxy, ' ').log("oxy")

    def main(args: Array[String]) = println((oxys ~ eoi.log("eoi"))("oxy oxy oxy"))
}

object ReParser extends MoreStringParsers {
    val number = chainl1("[1-9]".r, "[0-9]+".r, empty ^^ { _ => (x: String, y: String) => x + y })

    def main(args: Array[String]) = println(number("1239mfas"))
}


object DeclareParser extends StringParsers with MoreCombinators {

    def member =  "val".log("val") ~! "he".log("he")| "def".log("def") ~! "fuck".log("fuck")

    def main(args: Array[String]) = println((member.log("member") ~ eoi.log("eoi"))("valfe"))
}

object ArithmericParsers extends MoreStringParsers with MoreCombinators { self =>

    def number = chainl1("[1-9]".r, "[0-9]+".r, empty ^^ { _ => (x:String, y:String) => x + y})

    def double = (number ~ '.' ~ number) ^^ {case ((x, y), z) => x + y.toString + z} | number

    def plus = '+' ^^ {_ => (x: Double, z: Double) => x + z}
    def mins = '-' ^^ {_ => (x: Double, z: Double) => x - z}
    def prod = '*' ^^ {_ => (x: Double, z: Double) => x * z}
    def div = '/' ^^ {_ => (x: Double, z: Double) => x / z}

    def expr = chainl1(term, plus or mins)
    def term = chainl1(factor, prod or div)
    def factor: Parser[Double] = '(' ~> expr <~ ')' or double ^^ {_.toDouble}

    def main(args: Array[String]) = println(expr("13+12.5*4"))
}


