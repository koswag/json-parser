package json.typing

object ParserTypes {

    type Position = (Int, Int)

    type Input = (List[Char], Position)

    type Parsed[A] = (List[Char], A)
    type Result[A] = Option[Parsed[A]]

    type Output[A] = (Result[A], Position)


    implicit class PositionOps(val position: Position) {

        def <~<(chars: List[Char]): Position =
            chars.foldLeft(position) {
                (acc, char) => acc <~ char
            }

        def <~(char: Char): Position = {
            val (line, charNo) = position

            if (char == '\n')
                (line + 1, 1)
            else (line, charNo + 1)
        }

    }


    implicit class ResultOps[A](val result: Result[A]) {

        def mapValue[B](func: A => B): Result[B] =
            for {
                (rest, value) <- result
            } yield (rest, func(value))

    }


    implicit class OutputOps[A](val output: Output[A]) {

        def isDefined: Boolean = output._1.isDefined

        def mapValue[B](func: A => B): Output[B] =
            (output._1.mapValue(func), output._2)

        def mapResult[B](func: Result[A] => Result[B]): Output[B] =
            map2(func)(identity)

        def mapPosition(func: Position => Position): Output[A] =
            map2(identity)(func)

        def map2[B](funcRes: Result[A] => Result[B])(funcPos: Position => Position): Output[B] =
            (funcRes(output._1), funcPos(output._2))

        def map[B](func: A => B): Output[B] =
            (output._1.mapValue(func), output._2)

        def flatMap[B](func: A => Output[B]): Output[B] = {
            val (res, pos) = output

            val mapped = res match {
                case Some((_, value)) => func(value)
                case None => (None, pos)
            }

            (mapped._1, pos)
        }

    }

}
