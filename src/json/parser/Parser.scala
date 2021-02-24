package json.parser

import json.parser.Parser.Result

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}


trait Parser[A] extends (List[Char] => Result[A]) {

    /**
     * Creates a parser which applies the other parser if this one succeeds.
     *
     * @param other - Target parser
     * @tparam B - Target parser's result type
     * @return Combined parser
     */
    def *>[B](other: Parser[B]): Parser[B] =
        input => for {
            (rest, _) <- apply(input)
            result <- other(rest)
        } yield result

    /**
     * Resulting parser applies this parser if the other one succeeds.
     *
     * @param other Some other parser
     * @tparam B Other parser's result type
     * @return Combined parser
     */
    def <*[B](other: Parser[B]): Parser[A] =
        input => for {
            (rest, res) <- apply(input)
            (rest_, _) <- other(rest)
        } yield (rest_, res)

    /**
     * Creates a parser which applies the other parser if this one fails.
     *
     * @param other Parser following this one
     * @return Combined parser
     */
    def or(other: => Parser[A]): Parser[A] =
        input => {
            val res = apply(input)
            if (res.isDefined) res
            else other(input)
        }

    /**
     * Applies this parser as long as it succeeds and stores results in a list.
     *
     * @param input    - Input text as char list
     * @param elements - Element accumulator
     * @return List of results
     */
    @tailrec final def many(input: List[Char],
                            elements: List[A] = List()): Result[List[A]] =
        apply(input) match {
            case Some((rest, element)) =>
                val newElements = elements :+ element
                if (rest.nonEmpty)
                    many(rest, newElements)
                else Some(rest, newElements)
            case None =>
                if (elements.nonEmpty)
                    Some(input, elements)
                else None
        }

    /**
     * Resulting parser accepts chain of patterns separated by given pattern.
     *
     * @param sep Chain separator parser
     * @tparam B Separator type
     * @return Parser producing list of results
     */
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

    def surroundedBy[B](other: Parser[B]): Parser[A] =
        other *> this <* other

}


object Parser {

    type Position = (Int, Int)

    type Input = (List[Char], Position)

    type Parsed[A] = (List[Char], A)
    type Result[A] = Option[Parsed[A]]

    type Output[A] = (Result[A], Position)


    /**
     * Asserts that parser's result is a non empty list, fails otherwise.
     */
    def nonEmpty[A](parser: Parser[List[A]]): Parser[List[A]] =
        input => for {
            (rest, xs) <- parser(input)
            if xs.nonEmpty
        } yield (rest, xs)


    /**
     * Creates a parser producing an empty list.
     */
    def unit[A]: Parser[List[A]] =
        input => Some(input, List.empty)


    /**
     * Resulting parser produces an empty list on failure.
     */
    def optional[A](parser: Parser[A]): Parser[List[A]] =
        input => parser(input) match {
            case Some((rest, res)) =>
                Some(rest, List(res))
            case None =>
                unit(input)
        }


    /**
     * Parser accepting given character.
     *
     * @param char Character pattern
     */
    case class CharParser(char: Char) extends Parser[Char] {

        override def apply(input: List[Char]): Result[Char] =
            input match {
                case x :: xs
                    if x == char => Some(xs, char)
                case _ => None
            }

    }


    /**
     * Parser accepting given string of characters.
     *
     * @param str String pattern
     */
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

    /**
     * Parser accepting a two element pair.
     *
     * @param key       Leftmost pair element
     * @param separator Pair separator
     * @param value     Rightmost pair element
     * @return Pair parser
     */
    case class PairParser[K, V](key: Parser[K],
                                separator: Parser[Char],
                                value: Parser[V]) extends Parser[(K, V)] {

        override def apply(input: List[Char]): Result[(K, V)] =
            for {
                (rest, k) <- key(input)
                (rest_, _) <- separator(rest)
                (rest__, v) <- value(rest_)
            } yield (rest__, k -> v)

    }


    /**
     * Parser accepting characters as long as they match given predicate.
     *
     * Escape char mapping should contain pairs (letter -> escapeChar) e.g. ('n' -> '\n')
     *
     * @param pred        Character predicate
     * @param escapeChars Escape character mapping
     */
    case class SpanParser(pred: Char => Boolean,
                          escapeChars: Map[Char, Char] = Map()) extends Parser[List[Char]] {

        override def apply(input: List[Char]): Result[List[Char]] = {
            val (token, rest) = input.span(pred)

            if (escapeChars.nonEmpty && token.nonEmpty) {
                replaceEscapeChars(token) match {
                    case Success(value) => Some(rest, value)
                    case Failure(_) => None
                }
            } else
                Some(rest, token)
        }

        private def replaceEscapeChars(token: List[Char]): Try[List[Char]] =
            Try {
                token.foldLeft(
                    List(token.head)
                ) { (acc, ch) =>
                    if (acc.last == '\\')
                        acc.dropRight(1) :+ escapeChars(ch)
                    else
                        acc :+ ch
                }
            }

    }

}
