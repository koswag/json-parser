package parser

import parser.Parser.Result

import scala.annotation.tailrec


trait Parser[A] extends (List[Char] => Result[A])


object Parser {

    type Result[A] = Option[(List[Char], A)]


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
}
