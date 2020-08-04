package parser

object JsonTypes {

    sealed trait JsonValue {
        type A

        def value: A

        def as[T <: JsonValue]: T =
            this.asInstanceOf[T]
    }

    case object JsonNull extends JsonValue {
        override type A = Null

        override def value: A = null
    }

    case class JsonBool(value: Boolean) extends JsonValue {
        override type A = Boolean
    }

    case class JsonNumber(value: Int) extends JsonValue { // no Double
        override type A = Int
    }

    case class JsonString(value: String) extends JsonValue {
        override type A = String
    }

    case class JsonArray(value: List[JsonValue]) extends JsonValue {
        override type A = List[JsonValue]

        def getValues: List[Any] =
            valuesOf(this).asInstanceOf[List[Any]]
    }

    case class JsonObject(value: List[(List[Char], JsonValue)]) extends JsonValue {
        override type A = List[(List[Char], JsonValue)]

        def getValues: Map[String, Any] =
            valuesOf(this).asInstanceOf[Map[String, Any]]
    }

    def valuesOf(json: JsonValue): Any = json match {
        case obj: JsonObject =>
            obj.value.map {
                case (k, v) =>
                    k.mkString -> valuesOf(v)
            }
        case arr: JsonArray =>
            arr.value map valuesOf
        case _ => json.value
    }

}
