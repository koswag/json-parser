package parser

import parser.Parser.Result

import scala.annotation.tailrec


trait Parser[A] extends (List[Char] => Result[A]) {

    def *>[B](other: Parser[B]): Parser[B] =
        input => for {
            (rest, _) <- apply(input)
            result <- other(rest)
        } yield result

    def <*[B](other: Parser[B]): Parser[A] =
        input => for {
            (rest, res) <- apply(input)
            (rest_, _) <- other(rest)
        } yield (rest_, res)

    def or(other: => Parser[A]): Parser[A] =
        input => {
            val res = apply(input)
            if (res.isDefined) res
            else other(input)
        }

    def surroundedBy[B](parser: Parser[B]): Parser[A] =
        parser *> this <* parser

    @tailrec
    final def many(input: List[Char],
                   elements: List[A] = List()): Result[List[A]] =
        apply(input) match {
            case Some((rest, element)) =>
                val newElements = elements :+ element
                if (rest.nonEmpty)
                    many(rest, newElements)
                else Some(rest, newElements)
            case None =>
                if (elements.nonEmpty) {
                    Some(input, elements)
                } else None
        }

    def separatedBy[B](sep: Parser[B]): Parser[List[A]] =
        input => {
            val parseElement: Parser[A] =
                sep *> this

            apply(input) match {
                case Some((rest, elem)) =>
                    parseElement.many(rest, List(elem))
                case _ => None
            }
        }

}


object Parser {

    type Result[A] = Option[(List[Char], A)]

    def nonEmpty[A](parser: Parser[List[A]]): Parser[List[A]] =
        input => for {
            (input_, xs) <- parser(input)
            if xs.nonEmpty
        } yield (input_, xs)

    def empty[A]: Parser[List[A]] =
        input => Some(input, List.empty)


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
