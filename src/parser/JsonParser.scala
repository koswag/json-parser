package parser

import parser.JsonParser.{JSON_VALUE, LEFT_SQ_BRACKET, STRING_LITERAL}
import parser.JsonTypes.{JsonArray, JsonBool, JsonNull, JsonNumber, JsonString, JsonValue}
import parser.Parser.{CharParser, Result, SpanParser, StringParser, nonEmpty}

object JsonParser {

    private val COMMA = CharParser(',')
    private val QUOTE = CharParser('"')

    private val LEFT_SQ_BRACKET = CharParser('[')
    private val RIGHT_SQ_BRACKET = CharParser(']')

    private val SPACES = SpanParser(_.isWhitespace)
    private val LIST_SEPARATOR = COMMA surroundedBy SPACES

    private val STRING_LITERAL = SpanParser(_ != '"') surroundedBy QUOTE

    private val JSON_VALUE = (
        JsonNullParser
            or JsonBoolParser
            or JsonNumberParser
            or JsonStringParser
            or JsonArrayParser
        )

    case class KeywordParser(keyword: String) extends Parser[JsonValue] {

        private val keywordMapping: Map[String, JsonValue] = Map(
            "null" -> JsonNull,
            "true" -> JsonBool(true),
            "false" -> JsonBool(false)
        )

        override def apply(input: List[Char]): Result[JsonValue] = {
            if (keywordMapping contains keyword) {
                val value = keywordMapping(keyword)
                for {
                    (rest, _) <- StringParser(keyword)(input)
                } yield (rest, value)
            } else None
        }

    }


    object JsonNullParser extends Parser[JsonValue] {
        override def apply(input: List[Char]): Result[JsonValue] =
            KeywordParser("null")(input)
    }


    object JsonBoolParser extends Parser[JsonValue] {

        override def apply(input: List[Char]): Result[JsonValue] =
            (trueParser or falseParser) (input)

        private val trueParser: Parser[JsonValue] =
            KeywordParser("true")

        private val falseParser: Parser[JsonValue] =
            KeywordParser("false")

    }


    object JsonNumberParser extends Parser[JsonValue] {

        private val numParser =
            nonEmpty(SpanParser(_.isDigit))

        override def apply(input: List[Char]): Result[JsonValue] =
            for {
                (rest, token) <- numParser(input)
            } yield {
                val number = token.mkString.toInt
                (rest, JsonNumber(number))
            }

    }


    object JsonStringParser extends Parser[JsonValue] {

        override def apply(input: List[Char]): Result[JsonValue] =
            for {
                (rest, token) <- STRING_LITERAL(input)
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
            val elements = (JSON_VALUE sepBy LIST_SEPARATOR
                or Parser.empty)
            LEFT_SQ_BRACKET *> elements <* RIGHT_SQ_BRACKET
        }

    }

}
