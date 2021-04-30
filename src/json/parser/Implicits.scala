package json.parser

import json.parser.Parser.combineResults

object Implicits {


    implicit class ListParserOps[A](self: Parser[List[A]]) {

        def followedBy(next: Parser[A]): Parser[List[A]] =
            combineResults(self, next)(_ :+ _)

        def followedByMany(next: Parser[List[A]]): Parser[List[A]] =
            combineResults(self, next)(_ ++ _)

    }


    implicit class ElementParserOps[A](self: Parser[A]) {

        def followedBy(next: Parser[A]): Parser[List[A]] =
            combineResults(self, next)(List(_, _))

        def followedByMany(next: Parser[List[A]]): Parser[List[A]] =
            combineResults(self, next)(_ +: _)

    }

}
