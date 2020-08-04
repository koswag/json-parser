package parser

import parser.JsonTypes.{JsonBool, JsonNull, JsonNumber, JsonValue}
import parser.Parser.{Result, SpanParser, StringParser, nonEmpty}

object JsonParser {

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

}
