package parser

import parser.JsonTypes.{JsonBool, JsonNull, JsonValue}
import parser.Parser.{Result, StringParser}

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

}
