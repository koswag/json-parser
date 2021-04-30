package json

import json.parser.JsonParser
import json.typing.JsonTypes.JsonArray

import scala.io.Source
import scala.util.{Try, Using}

object Main extends App {

    def readFile(path: String): Try[String] =
        Using(Source.fromFile(path)) {
            file =>
                file.getLines().mkString
        }

    for (
        contents <- readFile("resources/test.json")
    ) {
        val (parsed, pos) = JsonParser(contents)

        parsed match {
            case Some((_, json)) =>
                json.as[JsonArray].getValues foreach println
            case None =>
                val (line, char) = pos
                println(s"Parsing failed: $line:$char")
        }
    }

}
