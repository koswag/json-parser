package parser

import parser.Implicits._
import parser.JsonTypes._
import parser.Parser._

object JsonParser {

    def apply(input: String): Result[JsonValue] = {
        val input_ = input.toList
        JSON_VALUE(input_)
    }

    private val COLON = CharParser(':')

    private val LEFT_SQ_BRACKET = CharParser('[')
    private val RIGHT_SQ_BRACKET = CharParser(']')

    private val LEFT_BRACE = CharParser('{')
    private val RIGHT_BRACE = CharParser('}')

    private val COMMA = CharParser(',')
    private val SPACES = SpanParser(_.isWhitespace)
    private val ELEMENT_SEPARATOR = SPACES *> COMMA <* SPACES

    private val QUOTE = CharParser('"')
    private val TEXT_TIL_QUOTE = SpanParser(_ != '"')
    private val STRING = QUOTE *> TEXT_TIL_QUOTE <* QUOTE

    private val MINUS = CharParser('-')
    private val DOT = CharParser('.')
    private val DIGITS = SpanParser(_.isDigit)
    private val POS_NUMBER = nonEmpty(DIGITS)
    private val NUMBER = optional(MINUS) followedByMany POS_NUMBER
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


    private case class KeywordParser(mapping: (String, JsonValue)) extends Parser[JsonValue] {

        private val (key, value) = mapping

        override def apply(input: List[Char]): Result[JsonValue] =
            for {
                (rest, _) <- StringParser(key)(input)
            } yield (rest, value)

    }


    /**
     * Parser accepting the <b>null</b> keyword.
     */
    object JsonNullParser extends Parser[JsonValue] {

        override def apply(input: List[Char]): Result[JsonValue] =
            nullParser(input)

        private val nullParser: Parser[JsonValue] =
            KeywordParser("null" -> JsonNull)

    }


    /**
     * Parser accepting the boolean values: <b>true</b> and <b>false</b>.
     */
    object JsonBoolParser extends Parser[JsonValue] {

        override def apply(input: List[Char]): Result[JsonValue] =
            (trueParser or falseParser) (input)

        private val trueParser: Parser[JsonValue] =
            KeywordParser("true" -> JsonBool(true))

        private val falseParser: Parser[JsonValue] =
            KeywordParser("false" -> JsonBool(false))

    }


    /**
     * Parser accepting integer numbers.
     */
    object JsonIntParser extends Parser[JsonValue] {

        override def apply(input: List[Char]): Result[JsonValue] =
            for {
                (rest, token) <- NUMBER(input)
            } yield {
                val number = token.mkString.toInt
                (rest, JsonInt(number))
            }

    }


    /**
     * Parser accepting floating point numbers.
     */
    object JsonDoubleParser extends Parser[JsonValue] {

        override def apply(input: List[Char]): Result[JsonValue] =
            for {
                (rest, token) <- FLOAT(input)
            } yield {
                val number = token.mkString.toDouble
                (rest, JsonDouble(number))
            }

    }


    /**
     * Parser accepting strings in double quotes.
     */
    object JsonStringParser extends Parser[JsonValue] {

        override def apply(input: List[Char]): Result[JsonValue] =
            for {
                (rest, token) <- STRING(input)
            } yield {
                val str = token.mkString("")
                (rest, JsonString(str))
            }

    }


    /**
     * Parser accepting an array of JSON values.
     */
    object JsonArrayParser extends Parser[JsonValue] {

        override def apply(input: List[Char]): Result[JsonValue] =
            for {
                (rest, tokens) <- parseElements(input)
            } yield {
                val elements = JsonArray(tokens)
                (rest, elements)
            }

        private val elements: Parser[List[JsonValue]] =
            (JSON_VALUE separatedBy ELEMENT_SEPARATOR) or Parser.empty

        private val parseElements: Parser[List[JsonValue]] =
            LEFT_SQ_BRACKET *> SPACES *> elements <* SPACES <* RIGHT_SQ_BRACKET

    }


    /**
     * Parser accepting a JSON object
     */
    object JsonObjectParser extends Parser[JsonValue] {

        override def apply(input: List[Char]): Result[JsonValue] =
            for {
                (rest, pairs) <- parseObject(input)
            } yield (rest, JsonObject(pairs))


        val pair: Parser[Pair] =
            PairParser(
                key = STRING,
                separator = SPACES *> COLON <* SPACES,
                value = JSON_VALUE
            )

        val parseObject: Parser[List[Pair]] =
            SPACES *> LEFT_BRACE *> SPACES *>
                (pair separatedBy ELEMENT_SEPARATOR or
                    Parser.empty) <*
                SPACES <* RIGHT_BRACE <* SPACES

    }

}
