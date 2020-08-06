package main

import parser.JsonParser
import parser.JsonTypes.JsonObject

import scala.io.Source
import scala.util.{Try, Using}


object Main extends App {

    def readFile(path: String): Try[String] =
        Using(Source.fromFile(path)) {
            file =>
                file.getLines().mkString
        }


    for (contents <- readFile("resources/short.json")) {

        val json: Option[JsonObject] =
            for {
                (_, json) <- JsonParser(contents)
            } yield json.as[JsonObject]

        println(json)

    }

}
