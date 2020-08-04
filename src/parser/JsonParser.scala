package parser

import parser.JsonTypes._
import parser.Parser._

object JsonParser {

    def apply(input: String): Result[JsonValue] = {
        val input_ = input.toList
        JSON_VALUE(input_)
    }


    private val COMMA = CharParser(',')
    private val QUOTE = CharParser('"')
    private val COLON = CharParser(':')

    private val LEFT_SQ_BRACKET = CharParser('[')
    private val RIGHT_SQ_BRACKET = CharParser(']')

    private val LEFT_BRACE = CharParser('{')
    private val RIGHT_BRACE = CharParser('}')

    private val SPACES = SpanParser(_.isWhitespace)
    private val LIST_SEPARATOR = COMMA surroundedBy SPACES

    private val STRING_LITERAL = SpanParser(_ != '"') surroundedBy QUOTE

    private val JSON_VALUE = (
        JsonNullParser
            or JsonBoolParser
            or JsonNumberParser
            or JsonStringParser
            or JsonArrayParser
            or JsonObjectParser
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
            val elements = (JSON_VALUE separatedBy LIST_SEPARATOR
                or Parser.empty)
            LEFT_SQ_BRACKET *> elements <* RIGHT_SQ_BRACKET
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
                STRING_LITERAL,
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
