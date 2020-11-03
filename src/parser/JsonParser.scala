package parser

import parser.Implicits._
import parser.JsonTypes._
import parser.Parser._

object JsonParser {

    def apply(input: String): Result[JsonValue] = {
        val input_ = input.toList
        JSON_VALUE(input_)
    }

    private val escapeChars = Map(
        'b' -> '\b',
        'f' -> '\f',
        'n' -> '\n',
        'r' -> '\r',
        't' -> '\t',
        '"' -> '\"',
        '\\' -> '\\',
    )

    private val COLON = CharParser(':')

    private val LEFT_SQ_BRACKET = CharParser('[')
    private val RIGHT_SQ_BRACKET = CharParser(']')

    private val LEFT_BRACE = CharParser('{')
    private val RIGHT_BRACE = CharParser('}')

    private val COMMA = CharParser(',')
    private val SPACES = SpanParser(_.isWhitespace)
    private val ELEMENT_SEPARATOR = SPACES *> COMMA <* SPACES

    private val QUOTE = CharParser('"')
    private val TEXT_TIL_QUOTE = SpanParser(_ != '"', escapeChars)
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


    /**
     * Parser accepting the <b>null</b> keyword.
     */
    private object JsonNullParser extends Parser[JsonValue] {

        override def apply(input: List[Char]): Result[JsonValue] =
            nullParser(input)

        private val nullParser: Parser[JsonValue] =
            KeywordParser("null" -> JsonNull)

    }


    /**
     * Parser accepting the boolean values: <b>true</b> and <b>false</b>.
     */
    private object JsonBoolParser extends Parser[JsonValue] {

        private val trueParser: Parser[JsonValue] =
            KeywordParser("true" -> JsonBool(true))

        private val falseParser: Parser[JsonValue] =
            KeywordParser("false" -> JsonBool(false))

        override def apply(input: List[Char]): Result[JsonValue] =
            (trueParser or falseParser) (input)

    }


    /**
     * Parser accepting integer numbers.
     */
    private object JsonIntParser extends Parser[JsonValue] {

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
    private object JsonDoubleParser extends Parser[JsonValue] {

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
    private object JsonStringParser extends Parser[JsonValue] {

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
    private object JsonArrayParser extends Parser[JsonValue] {

        private val elements: Parser[List[JsonValue]] =
            (JSON_VALUE separatedBy ELEMENT_SEPARATOR) or Parser.empty

        private val parseElements: Parser[List[JsonValue]] =
            SPACES *> LEFT_SQ_BRACKET *> SPACES *>
                elements <* SPACES <* RIGHT_SQ_BRACKET <* SPACES

        override def apply(input: List[Char]): Result[JsonValue] =
            for {
                (rest, tokens) <- parseElements(input)
            } yield {
                val elements = JsonArray(tokens)
                (rest, elements)
            }

    }


    /**
     * Parser accepting a JSON object
     */
    private object JsonObjectParser extends Parser[JsonValue] {

        private val property: Parser[JsonProperty] =
            PairParser(
                key = STRING,
                separator = SPACES *> COLON <* SPACES,
                value = JSON_VALUE
            )

        private val properties: Parser[List[JsonProperty]] =
            property separatedBy ELEMENT_SEPARATOR

        val parseObject: Parser[List[JsonProperty]] =
            SPACES *> LEFT_BRACE *> SPACES *>
                (properties or Parser.empty) <* SPACES <* RIGHT_BRACE <* SPACES

        override def apply(input: List[Char]): Result[JsonValue] =
            for {
                (rest, pairs) <- parseObject(input)
            } yield (rest, JsonObject(pairs))

    }


    private class KeywordParser(key: String, value: JsonValue) extends Parser[JsonValue] {

        private val parser: StringParser =
            StringParser(key)

        override def apply(input: List[Char]): Result[JsonValue] =
            for {
                (rest, _) <- parser(input)
            } yield (rest, value)

    }


    private object KeywordParser {

        def apply(mapping: (String, JsonValue)): KeywordParser = {
            val (key, value) = mapping
            new KeywordParser(key, value)
        }

    }

}
