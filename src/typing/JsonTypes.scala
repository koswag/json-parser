package typing

object JsonTypes {

    type JsonProperty = (List[Char], JsonValue)

    sealed trait JsonValue {
        type A <: JsonSerializable[_]

        def value: A

        def as[T <: JsonValue]: T =
            this.asInstanceOf[T]

        def get: Any =
            toScalaValue(this)
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
            value map toScalaValue
    }

    case class JsonObject(value: List[JsonProperty]) extends JsonValue {
        override type A = List[JsonProperty]

        def getPairs: Map[String, Any] = value.map({
            case (key, value_) =>
                (key.mkString, toScalaValue(value_))
        }).toMap
    }

    /**
     * Maps JSON value to its Scala counterpart.
     *
     * @return One of basic Scala types:
     *
     *         Null | Boolean | Int | Double | String | List | Map
     */
    def toScalaValue(jsonValue: JsonValue): Any = jsonValue match {
        case obj: JsonObject => obj.getPairs
        case arr: JsonArray => arr.getValues
        case other => other.value
    }

}
