package json.parser

import json.parser.Parser.extractOutput
import json.typing.ParserTypes._

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}


trait Parser[A] extends (Input => Output[A]) {

    /**
     * Creates a parser which applies the other parser if this one succeeds.
     *
     * @param other - Target parser
     * @tparam B - Target parser's result type
     * @return Combined parser
     */
    def *>[B](other: Parser[B]): Parser[B] = {
        input =>
            val (res, pos) = apply(input)

            val parsed = for {
                (rest, _) <- res

                (res_, pos_) = other(rest, pos)
                (rest_, value) <- res_
            } yield ((rest_, value), pos_)

            extractOutput(parsed, pos)
    }

    /**
     * Resulting parser applies this parser if the other one succeeds.
     *
     * @param other Some other parser
     * @tparam B Other parser's result type
     * @return Combined parser
     */
    def <*[B](other: Parser[B]): Parser[A] = {
        input =>
            val (res, pos) = apply(input)

            val parsed = for {
                (rest, value) <- res

                (res_, pos_) = other(rest, pos)
                (rest_, _) <- res_
            } yield ((rest_, value), pos_)

            extractOutput(parsed, pos)
    }

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
    final def many(input: Input,
                   elements: List[A] = List()): Output[List[A]] = {
        val (chars, _) = input
        val (res, pos) = apply(input)

        res match {
            case Some((rest, element)) =>
                val newElements = elements :+ element
                if (rest.nonEmpty)
                    many((rest, pos), newElements)
                else (Some(rest, newElements), pos)
            case None =>
                if (elements.nonEmpty)
                    (Some(chars, elements), pos)
                else (None, pos)
        }
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

            val (_, pos) = input

            apply(input) match {
                case (Some((rest, elem)), newPos) =>
                    parseElement.many((rest, newPos), List(elem))
                case _ => (None, pos)
            }
        }

}


object Parser {

    def extractOutput[A](parsed: Option[(Parsed[A], Position)], position: Position): Output[A] =
        parsed match {
            case Some((result, pos)) =>
                (Some(result), pos)
            case None => (None, position)
        }

    /**
     * Resulting parser asserts that parser's result is a non empty list, fails otherwise.
     */
    def nonEmpty[A](parser: Parser[List[A]]): Parser[List[A]] =
        input =>
            parser(input) mapResult { result =>
                for {
                    (rest, xs) <- result
                    if xs.nonEmpty
                } yield (rest, xs)
            }

    /**
     * Creates a parser producing an empty list.
     */
    def unit[A]: Parser[List[A]] = {
        case (input, position) =>
            (Some(input, List.empty), position)
    }


    /**
     * Resulting parser produces an empty list on failure.
     */
    def optional[A](parser: Parser[A]): Parser[List[A]] =
        input => parser(input) match {
            case (Some((rest, res)), position) =>
                (Some(rest, List(res)), position)
            case (None, _) =>
                unit(input)
        }


    def combineResults[A, B, C](p1: Parser[A], p2: Parser[B])(transform: (A, B) => C): Parser[C] = {
        input =>
            val (res, pos) = p1(input)

            val parsed = for {
                (rest, first) <- res

                (res_, pos_) = p2(rest, pos)
                (rest_, second) <- res_
            } yield ((rest_, transform(first, second)), pos_)

            extractOutput(parsed, pos)
    }


    /**
     * Parser accepting given character.
     *
     * @param char Character pattern
     */
    case class CharParser(char: Char) extends Parser[Char] {

        override def apply(input: Input): Output[Char] = {
            val (chars, position) = input

            chars match {
                case x :: xs if x == char =>
                    (Some(xs, char), position <~ char)
                case _ => (None, position <~ char)
            }
        }

    }


    /**
     * Parser accepting given string of characters.
     *
     * @param str String pattern
     */
    case class StringParser(str: String) extends Parser[List[Char]] {

        override def apply(input: Input): Output[List[Char]] =
            parseString(str.toList, input)

        @tailrec
        private def parseString(str: List[Char],
                                input: Input,
                                acc: List[Char] = List()): Output[List[Char]] = {
            val (chars, position) = input

            if (str.isEmpty)
                (Some(chars, acc), position)
            else
                CharParser(str.head)(input) match {
                    case (Some((rest, ch)), position) =>
                        parseString(str.tail, (rest, position), acc :+ ch)
                    case (_, pos) => (None, pos)
                }
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

        // FIXME: In case of failure position is pointing to the beggining of the pair
        override def apply(input: Input): Output[(K, V)] = {
            val (parsedKey, pos) = key(input)

            val parsed = for {
                (rest, k) <- parsedKey

                (parsedSep, pos_) = separator(rest, pos)
                (rest_, _) <- parsedSep

                (parsedVal, pos__) = value(rest_, pos_)
                (rest__, v) <- parsedVal
            } yield ((rest__, k -> v), pos__)

            extractOutput(parsed, pos)
        }

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

        override def apply(input: Input): Output[List[Char]] = {
            val (chars, pos) = input

            val (token, rest) = chars.span(pred)

            if (escapeChars.nonEmpty && token.nonEmpty) {
                replaceEscapeChars(token) match {
                    case Success(replaced) =>
                        (Some(rest, replaced), pos <~< replaced)
                    case Failure(_) => (None, pos)
                }
            } else (Some(rest, token), pos <~< token)
        }

        private def replaceEscapeChars(token: List[Char]): Try[List[Char]] =
            Try {
                token.foldLeft(
                    List(token.head)
                ) { (acc, ch) =>
                    if (acc.last == '\\')
                        acc.dropRight(1) :+ escapeChars(ch)
                    else acc :+ ch
                }
            }

    }

}
