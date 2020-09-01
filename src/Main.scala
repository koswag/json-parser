import parser.JsonParser
import parser.JsonTypes.JsonArray

import scala.io.Source
import scala.util.{Try, Using}


object Main extends App {

    def readFile(path: String): Try[String] =
        Using(Source.fromFile(path)) {
            file =>
                file.getLines().mkString
        }


    for (contents <- readFile("resources/test.json")) {
//        for ((_, json) <- JsonParser(contents)) {
//            json.as[JsonArray].getValues foreach println
//        }
        JsonParser(contents) match {
            case Some((_, json)) =>
                json.as[JsonArray].getValues foreach println
            case None =>
                println("Parsing failed")
        }
    }

}
