package parser

import parser.JsonTypes.{JsonValue, Pair}
import parser.Parser.Result

import scala.annotation.tailrec


trait Parser[A] extends (List[Char] => Result[A]) {

    /**
     * Creates a parser which applies the other parser if this one succeeds.
     *
     * @param other - target parser
     * @tparam B - target parser result type
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
     * @param other Target parser
     * @tparam B Target parser result type
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

}


object Parser {

    type Result[A] = Option[(List[Char], A)]

    /**
     * Asserts that parser's result is not empty, fails otherwise.
     */
    def nonEmpty[A](parser: Parser[List[A]]): Parser[List[A]] =
        input => for {
            (rest, xs) <- parser(input)
            if xs.nonEmpty
        } yield (rest, xs)

    /**
     * Creates a parser producing an empty list.
     */
    def empty[A]: Parser[List[A]] =
        input => Some(input, List.empty)

    /**
     * Resulting parser produces an empty list on failure.
     */
    def optional[A](parser: Parser[A]): Parser[List[A]] =
        input => parser(input) match {
            case Some((rest, res)) =>
                Some(rest, List(res))
            case None =>
                empty(input)
        }


    /**
     * Parser accepting given character.
     *
     * @param char Character pattern
     */
    case class CharParser(char: Char) extends Parser[Char] {

        override def apply(input: List[Char]): Result[Char] = input match {
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
    case class PairParser(key: Parser[List[Char]],
                          separator: Parser[Char],
                          value: Parser[JsonValue]) extends Parser[Pair] {

        override def apply(input: List[Char]): Result[(List[Char], JsonValue)] =
            for {
                (rest, k) <- key(input)
                (rest_, _) <- separator(rest)
                (rest__, v) <- value(rest_)
            } yield (rest__, k -> v)

    }


    /**
     * Parser accepting characters as long as they match given predicate.
     *
     * @param pred Character predicate
     */
    case class SpanParser(pred: Char => Boolean) extends Parser[List[Char]] {

        override def apply(input: List[Char]): Result[List[Char]] = {
            val (token, rest) = input.span(pred)
            Some(rest, token)
        }

    }

}
