package parsercombinators.parsering

import scala.util.parsing.combinator.Parsers

/**
  * Created by ariwaranosai on 15/12/10.
  *
  */

object XMLParser extends Parsers {
    type Elem = Char

    trait Node
    case class ContainerNode(name: String, content: List[Node]) extends Node
    case class TextNode(content: String) extends Node

    def str1(what: String, pred: Char => Boolean) = rep1(elem(what, pred)) ^^ {_.mkString}

    def openTag: Parser[String] = '<' ~> str1("tag name", _.isLetter) <~ '>'

    def endTag(name: String) = ('<' ~ '/') ~> accept(name.toList) <~ '>'

    def xmlText: Parser[Node] = str1("xml text", {c => !(c == '<' || c == '>')}) ^^ TextNode

    def xml: Parser[Node] =
        (openTag into {name => rep(xml) <~ endTag(name) ^^ (ContainerNode(name, _))}) | xmlText


    import scala.util.parsing.input.CharArrayReader

    override def phrase[T](p: Parser[T]): Parser[T] = p

    def main(args: Array[String]) {
        println(phrase(xml)(new CharArrayReader("<person><name>bold</name><age>34</age></person>".toArray)))
    }

}
