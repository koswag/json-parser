package parser

import parser.Parser.Result

import scala.annotation.tailrec


trait Parser[A] extends (List[Char] => Result[A]) {

    def or(other: => Parser[A]): Parser[A] =
        input => {
            val res = apply(input)
            if (res.isDefined) res
            else other(input)
        }

}


object Parser {

    type Result[A] = Option[(List[Char], A)]

    def nonEmpty[A](parser: Parser[List[A]]): Parser[List[A]] =
        input => for {
            (input_, xs) <- parser(input)
            if xs.nonEmpty
        } yield (input_, xs)


    case class CharParser(char: Char) extends Parser[Char] {

        override def apply(input: List[Char]): Result[Char] = input match {
            case x :: xs
                if x == char => Some(xs, char)
            case _ => None
        }

    }


    case class StringParser(str: String) extends Parser[List[Char]] {

        override def apply(input: List[Char]): Result[List[Char]] =
            parseString(str.toList, input)

        @tailrec
        private def parseString(str: List[Char],
                                input: List[Char],
                                chars: List[Char] = List()): Result[List[Char]] =
            if (str.isEmpty)
                Some(input, chars)
            else
                CharParser(str.head)(input) match {
                    case Some((rest, ch)) =>
                        parseString(str.tail, rest, chars :+ ch)
                    case _ => None
                }

    }


    case class SpanParser(pred: Char => Boolean) extends Parser[List[Char]] {

        override def apply(input: List[Char]): Result[List[Char]] = {
            val (token, rest) = input.span(pred)
            Some(rest, token)
        }

    }
}
