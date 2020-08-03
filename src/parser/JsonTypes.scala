package parser

object JsonTypes {

    sealed trait JsonValue

    case object JsonNull
        extends JsonValue

    case class JsonBool(value: Boolean)
        extends JsonValue

    case class JsonNumber(value: Int)
        extends JsonValue

    case class JsonString(value: String)
        extends JsonValue

    case class JsonArray(value: List[JsonValue])
        extends JsonValue

    case class JsonObject(value: List[(List[Char], JsonValue)])
        extends JsonValue

}
