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
    }

    case class JsonObject(value: List[(List[Char], JsonValue)]) extends JsonValue {
        override type A = List[(List[Char], JsonValue)]
    }

}
