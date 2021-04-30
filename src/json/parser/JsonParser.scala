package json.parser

import json.parser.Implicits._
import json.typing.JsonTypes._
import json.parser.Parser._
import json.typing.ParserTypes._

object JsonParser {

    def apply(input: String): Output[JsonValue] = {
        val input_ = input.toList
        JSON_VALUE((input_, (1, 1)))
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

    private val SPACES = SpanParser(_.isWhitespace)

    private implicit class ParserExtension[A](value: Parser[A]) {
        def withSpaces: Parser[A] =
            SPACES *> value <* SPACES
    }

    private val COLON = CharParser(':').withSpaces

    private val LEFT_SQ_BRACKET = CharParser('[').withSpaces
    private val RIGHT_SQ_BRACKET = CharParser(']').withSpaces

    private val LEFT_BRACE = CharParser('{').withSpaces
    private val RIGHT_BRACE = CharParser('}').withSpaces

    private val COMMA = CharParser(',').withSpaces

    private val QUOTE = CharParser('"')
    private val TEXT_TIL_QUOTE = SpanParser(_ != '"', escapeChars)
    private val STRING = QUOTE *> TEXT_TIL_QUOTE <* QUOTE

    private val MINUS = CharParser('-')
    private val DOT = CharParser('.')
    private val DIGITS = SpanParser(_.isDigit)
    private val NUMBER = nonEmpty(DIGITS)
    private val INTEGER = optional(MINUS) followedByMany NUMBER
    private val FLOAT = INTEGER followedBy DOT followedByMany NUMBER

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

        override def apply(input: Input): Output[JsonValue] =
            nullParser(input)

        private val nullParser: Parser[JsonValue] =
            KeywordParser("null" -> JsonNull)

    }


    /**
     * Parser accepting boolean values: <b>true</b> and <b>false</b>.
     */
    private object JsonBoolParser extends Parser[JsonValue] {

        private val trueParser: Parser[JsonValue] =
            KeywordParser("true" -> JsonBool(true))

        private val falseParser: Parser[JsonValue] =
            KeywordParser("false" -> JsonBool(false))

        private val boolParser: Parser[JsonValue] =
            trueParser or falseParser


        override def apply(input: Input): Output[JsonValue] =
            boolParser(input)

    }


    /**
     * Parser accepting integer numbers.
     */
    private object JsonIntParser extends Parser[JsonValue] {

        override def apply(input: Input): Output[JsonValue] =
            INTEGER(input) mapValue {
                token =>
                    val number = token.mkString.toInt
                    JsonInt(number)
            }

    }


    /**
     * Parser accepting floating point numbers.
     */
    private object JsonDoubleParser extends Parser[JsonValue] {

        override def apply(input: Input): Output[JsonValue] = {
            FLOAT(input)
                .mapValue(_.mkString("").toDouble)
                .mapValue(JsonDouble)
        }

    }


    /**
     * Parser accepting strings in double quotes.
     */
    private object JsonStringParser extends Parser[JsonValue] {

        override def apply(input: Input): Output[JsonValue] =
            STRING(input)
                .mapValue(_.mkString(""))
                .mapValue(JsonString)

    }


    /**
     * Parser accepting an array of JSON values.
     */
    private object JsonArrayParser extends Parser[JsonValue] {

        private val elements: Parser[List[JsonValue]] =
            (JSON_VALUE separatedBy COMMA) or Parser.unit

        private val parseElements: Parser[List[JsonValue]] =
            LEFT_SQ_BRACKET *> elements <* RIGHT_SQ_BRACKET

        override def apply(input: Input): Output[JsonValue] =
            parseElements(input) mapValue JsonArray

    }


    /**
     * Parser accepting a JSON object
     */
    private object JsonObjectParser extends Parser[JsonValue] {

        private val property: Parser[JsonProperty] =
            PairParser(
                key = STRING,
                separator = COLON,
                value = JSON_VALUE
            )

        private val properties: Parser[List[JsonProperty]] =
            property separatedBy COMMA

        val parseObject: Parser[List[JsonProperty]] =
            LEFT_BRACE *> (properties or Parser.unit) <* RIGHT_BRACE

        override def apply(input: Input): Output[JsonValue] =
            parseObject(input) mapValue JsonObject

    }


    private class KeywordParser(key: String, value: JsonValue) extends Parser[JsonValue] {

        private val parser: StringParser =
            StringParser(key)

        override def apply(input: Input): Output[JsonValue] =
            parser(input) mapValue (_ => value)

    }


    private object KeywordParser {

        def apply(mapping: (String, JsonValue)): KeywordParser = {
            val (key, value) = mapping
            new KeywordParser(key, value)
        }

    }

}
