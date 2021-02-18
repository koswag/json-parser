package parser

import parser.JsonTypes._

object CheckedConversions {

    implicit class JsonSerializableExtension[A: JsonSerializable](val self: A) {

        def asJsonValue: JsonValue =
            self match {
                case null => JsonNull
                case bool: Boolean => JsonBool(bool)
                case int: Int => JsonInt(int)
                case double: Double => JsonDouble(double)
                case string: String => JsonString(string)
                case list: List[A] => list.asJson
                case map: Map[String, A] => map.asJson
            }

    }


    implicit class JsonSerializableListExtension[T: JsonSerializable](val self: List[T]) {

        def asJson: JsonArray =
            JsonArray(self.toJsonValues)

        def toJsonValues: List[JsonValue] =
            self.map(_.asJsonValue)

    }


    implicit class MapExtension[T: JsonSerializable](val self: Map[String, T]) {

        def asJson: JsonObject =
            JsonObject(self.toJsonProperties)

        def toJsonProperties: List[JsonProperty] =
            self.toList.map(_.toJsonProperty)

    }


    implicit class JsonPropertyExtension[T: JsonSerializable](val self: (String, T)) {

        def toJsonProperty: JsonProperty =
            self match {
                case (key, value) =>
                    (key.toList, value.asJsonValue)
            }

    }

}
