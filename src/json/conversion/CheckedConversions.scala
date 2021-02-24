package json.conversion

import json.typing.JsonSerializable
import json.typing.JsonTypes._

object CheckedConversions {

    implicit class JsonSerializableExtension[A: JsonSerializable](val value: A) {

        def asJsonValue: JsonValue =
            value match {
                case null => JsonNull
                case bool: Boolean => JsonBool(bool)
                case int: Int => JsonInt(int)
                case double: Double => JsonDouble(double)
                case string: String => JsonString(string)
                case list: List[A] => list.asJson
                case map: Map[String, A] => map.asJson
            }

    }


    implicit class JsonSerializableListExtension[T: JsonSerializable](val value: List[T]) {

        def asJson: JsonArray =
            JsonArray(value.toJsonValues)

        def toJsonValues: List[JsonValue] =
            value.map(_.asJsonValue)

    }


    implicit class MapExtension[T: JsonSerializable](val value: Map[String, T]) {

        def asJson: JsonObject =
            JsonObject(value.toJsonProperties)

        def toJsonProperties: List[JsonProperty] =
            value.toList.map(_.toJsonProperty)

    }


    implicit class JsonPropertyExtension[T: JsonSerializable](val value: (String, T)) {

        def toJsonProperty: JsonProperty =
            value match {
                case (key, value) =>
                    (key.toList, value.asJsonValue)
            }

    }

}
