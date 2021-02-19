import parser.JsonParser
import typing.JsonTypes.JsonArray
import conversion.CheckedConversions._

import scala.io.Source
import scala.util.{Try, Using}


object Main extends App {

    def readFile(path: String): Try[String] =
        Using(Source.fromFile(path)) {
            file =>
                file.getLines().mkString
        }

    for (
        contents <- readFile("resources/long.json")
    ) {
        JsonParser(contents) match {
            case Some((_, json)) =>
                json.as[JsonArray].getValues foreach println
            case None =>
                println("Parsing failed")
        }
    }

}
