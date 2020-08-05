package parser

object Implicits {

    implicit class ListParserOps[A](self: Parser[List[A]]) {

        def followedBy(next: Parser[A]): Parser[List[A]] =
            input => for {
                (rest, left) <- self(input)
                (rest_, right) <- next(rest)
            } yield (rest_, left :+ right)

        def followedByMany(next: Parser[List[A]]): Parser[List[A]] =
            input => for {
                (rest, left) <- self(input)
                (rest_, right) <- next(rest)
            } yield (rest_, left ++ right)

    }


    implicit class ElementParserOps[A](self: Parser[A]) {

        def followedBy(next: Parser[A]): Parser[List[A]] =
            input => for {
                (rest, left) <- self(input)
                (rest_, right) <- next(rest)
            } yield (rest_, List(left, right))

        def followedByMany(next: Parser[List[A]]): Parser[List[A]] =
            input => for {
                (rest, left) <- self(input)
                (rest_, right) <- next(rest)
            } yield (rest_, left +: right)

    }

}
