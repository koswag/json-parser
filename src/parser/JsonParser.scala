package parser

import parser.JsonTypes._
import parser.Parser._
import parser.Implicits._

object JsonParser {

    def apply(input: String): Result[JsonValue] = {
        val input_ = input.toList
        JSON_VALUE(input_)
    }


    private val COMMA = CharParser(',')
    private val QUOTE = CharParser('"')
    private val COLON = CharParser(':')
    private val DOT = CharParser('.')
    private val MINUS = CharParser('-')

    private val LEFT_SQ_BRACKET = CharParser('[')
    private val RIGHT_SQ_BRACKET = CharParser(']')

    private val LEFT_BRACE = CharParser('{')
    private val RIGHT_BRACE = CharParser('}')

    private val SPACES = SpanParser(_.isWhitespace)
    private val LIST_SEPARATOR = COMMA surroundedBy SPACES

    private val TEXT_TIL_QUOTE = SpanParser(_ != '"')
    private val STRING = TEXT_TIL_QUOTE surroundedBy QUOTE

    private val POS_NUMBER = nonEmpty(SpanParser(_.isDigit))
    private val NUMBER = optional(MINUS) followedByMany  POS_NUMBER
    private val FLOAT = NUMBER followedBy DOT followedByMany POS_NUMBER

    private val JSON_VALUE = (
        JsonNullParser
            or JsonBoolParser
            or JsonDoubleParser
            or JsonIntParser
            or JsonStringParser
            or JsonArrayParser
            or JsonObjectParser
        )


    case class KeywordParser(mapping: (String, JsonValue)) extends Parser[JsonValue] {

        override def apply(input: List[Char]): Result[JsonValue] = {
            val (key, value) = mapping
            for {
                (rest, _) <- StringParser(key)(input)
            } yield (rest, value)
        }

    }


    object JsonNullParser extends Parser[JsonValue] {

        private val nullParser: Parser[JsonValue] =
            KeywordParser("null" -> JsonNull)

        override def apply(input: List[Char]): Result[JsonValue] =
            nullParser(input)

    }


    object JsonBoolParser extends Parser[JsonValue] {

        private val trueParser: Parser[JsonValue] =
            KeywordParser("true" -> JsonBool(true))

        private val falseParser: Parser[JsonValue] =
            KeywordParser("false" -> JsonBool(false))

        override def apply(input: List[Char]): Result[JsonValue] =
            (trueParser or falseParser) (input)

    }


    object JsonIntParser extends Parser[JsonValue] {

        override def apply(input: List[Char]): Result[JsonValue] =
            for {
                (rest, token) <- NUMBER(input)
            } yield {
                val number = token.mkString.toInt
                (rest, JsonInt(number))
            }

    }


    object JsonDoubleParser extends Parser[JsonValue] {

        override def apply(input: List[Char]): Result[JsonValue] =
            for {
                (rest, token) <- FLOAT(input)
            } yield {
                val number = token.mkString.toDouble
                (rest, JsonDouble(number))
            }

    }


    object JsonStringParser extends Parser[JsonValue] {

        override def apply(input: List[Char]): Result[JsonValue] =
            for {
                (rest, token) <- STRING(input)
            } yield {
                val str = token.mkString("")
                (rest, JsonString(str))
            }

    }


    object JsonArrayParser extends Parser[JsonValue] {

        override def apply(input: List[Char]): Result[JsonValue] =
            for {
                (rest, tokens) <- parseElements(input)
            } yield {
                val elements = JsonArray(tokens)
                (rest, elements)
            }

        val parseElements: Parser[List[JsonValue]] = {
            val elements =
                (JSON_VALUE separatedBy LIST_SEPARATOR) or Parser.empty

            LEFT_SQ_BRACKET *> SPACES *> elements <* SPACES <* RIGHT_SQ_BRACKET
        }

    }


    object JsonObjectParser extends Parser[JsonValue] {

        type Pair = (List[Char], JsonValue)

        override def apply(input: List[Char]): Result[JsonValue] =
            for {
                (rest, pairs) <- parseObject(input)
            } yield (rest, JsonObject(pairs))

        val pair: Parser[Pair] =
            extractPair(
                STRING,
                COLON surroundedBy SPACES,
                JSON_VALUE
            )

        def extractPair(keyParser: Parser[List[Char]],
                        sepParser: Parser[Char],
                        valParser: Parser[JsonValue]): Parser[Pair] =
            input => for {
                (rest, k) <- keyParser(input)
                (rest_, _) <- sepParser(rest)
                (rest__, v) <- valParser(rest_)
            } yield (rest__, k -> v)

        val parseObject: Parser[List[Pair]] =
            SPACES *> LEFT_BRACE *> SPACES *>
                (pair separatedBy LIST_SEPARATOR or
                    Parser.empty) <*
                SPACES <* RIGHT_BRACE <* SPACES

    }

}
