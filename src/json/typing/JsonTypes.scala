package json.typing

object JsonTypes {

    type JsonProperty = (List[Char], JsonValue)


    sealed trait JsonValue {
        type A

        def value: A

        def as[T <: JsonValue]: T =
            this.asInstanceOf[T]

        /**
         * Converts JSON value to its Scala counterpart.
         *
         * @return One of basic Scala types:
         *
         *         Null | Boolean | Int | Double | String | List | Map
         */
        def toScala: Any =
            this match {
                case obj: JsonObject => obj.getPairs
                case arr: JsonArray => arr.getValues
                case other => other.value
            }

        def serialize: String = value.toString

    }


    case object JsonNull extends JsonValue {
        override type A = Null

        override def value: A = null
    }


    case class JsonBool(value: Boolean) extends JsonValue {
        override type A = Boolean
    }


    case class JsonInt(value: Int) extends JsonValue {
        override type A = Int
    }


    case class JsonDouble(value: Double) extends JsonValue {
        override type A = Double
    }


    case class JsonString(value: String) extends JsonValue {
        override type A = String

        override def serialize: String = s""""$value""""
    }


    case class JsonArray(value: List[JsonValue]) extends JsonValue {
        override type A = List[JsonValue]

        def getValues: List[Any] =
            value map (_.toScala)

        override def serialize: String =
            s"[${
                value.map(_.serialize)
                    .mkString(", ")
            }]"
    }


    case class JsonObject(value: List[JsonProperty]) extends JsonValue {
        override type A = List[JsonProperty]

        def getPairs: Map[String, Any] =
            value.map {
                case (key, value_) =>
                    (key.mkString, value_.toScala)
            }.toMap


        override def serialize: String =
            s"{${
                value.map(propToString)
                    .mkString(", ")
            }}"

        private def propToString(prop: JsonProperty): String =
            s""""${prop._1.mkString}": ${prop._2.serialize}"""
    }

}
