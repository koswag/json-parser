package json.conversion

import json.typing.JsonTypes._

object UncheckedConversions {

    implicit class AnyTypeExtension(val self: Any) {

        def asJsonValue: Option[JsonValue] =
            self match {
                case null => Some(JsonNull)
                case bool: Boolean => Some(JsonBool(bool))
                case int: Int => Some(JsonInt(int))
                case double: Double => Some(JsonDouble(double))
                case string: String => Some(JsonString(string))
                case list: List[_] => list.asJson
                case map: Map[String, _] => map.asJson
                case _ => None
            }

    }


    implicit class ListExtension(val self: List[_]) {

        def asJson: Option[JsonArray] =
            self.toJsonValues match {
                case Some(values) =>
                    Some(JsonArray(values))
                case _ => None
            }

        def toJsonValues: Option[List[JsonValue]] =
            self.map(_.asJsonValue)
                .foldRight(
                    Some(Nil): Option[List[JsonValue]]
                ) {
                    case (Some(value), Some(list)) =>
                        Some(value :: list)
                    case _ => None
                }

    }

    implicit class MapExtension(val self: Map[String, _]) {

        def asJson: Option[JsonObject] =
            self.toJsonProperties match {
                case Some(properties) =>
                    Some(JsonObject(properties))
                case _ => None
            }

        def toJsonProperties: Option[List[JsonProperty]] =
            self.toList.map(_.toJsonProperty)
                .foldRight(
                    Some(Nil): Option[List[JsonProperty]]
                ) {
                    case (Some(property), Some(list)) =>
                        Some(property :: list)
                    case _ => None
                }

    }


    implicit class JsonPropertyExtension(val self: (String, _)) {

        def toJsonProperty: Option[JsonProperty] = {
            val (key, value) = self

            value.asJsonValue match {
                case Some(value) =>
                    Some(key.toList, value)
                case _ => None
            }
        }

    }

}
