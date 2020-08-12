package parser

object JsonTypes {

    type Pair = (List[Char], JsonValue)

    sealed trait JsonValue {
        type A

        def value: A

        def as[T <: JsonValue]: T =
            this.asInstanceOf[T]

        def get: Any =
            mapToValue(this)
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
    }

    case class JsonArray(value: List[JsonValue]) extends JsonValue {
        override type A = List[JsonValue]

        def getValues: List[Any] =
            value.map(mapToValue)
    }

    case class JsonObject(value: List[Pair]) extends JsonValue {
        override type A = List[Pair]

        def getPairs: Map[String, Any] = value.map({
            case (key, value_) => (key.mkString, mapToValue(value_))
        }).toMap
    }

    /**
     * Maps JSON value to its Scala counterpart.
     *
     * @return One of types:
     *
     *         Null | Boolean | Int | Double | String | List | Map
     */
    def mapToValue(jsonValue: JsonValue): Any = jsonValue match {
        case obj: JsonObject => obj.getPairs
        case arr: JsonArray => arr.getValues
        case other => other.value
    }

}
