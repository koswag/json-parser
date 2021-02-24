package test

import json.conversion.UncheckedConversions._

object ConversionTest extends App {

    println(
        Map(
            "key" -> "value",
            "props" -> List(
                "red",
                "tall"
            ),
            "guaranteed" -> true
        ).asJson map (_.serialize)
    )

}
