package main

import parser.JsonParser

import scala.io.Source
import scala.util.{Try, Using}


object Main extends App {

    def readFile(path: String): Try[String] =
        Using(Source.fromFile(path)) {
            file =>
                file.getLines().mkString
        }


    for (contents <- readFile("resources/long.json")) {
        for {
            (_, json) <- JsonParser(contents)
        } println(json)
    }

}
