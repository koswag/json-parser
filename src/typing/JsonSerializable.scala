package typing

sealed trait JsonSerializable[-A]

object JsonSerializable {

    implicit object NullWitness extends JsonSerializable[Null]

    implicit object BoolWitness extends JsonSerializable[Boolean]

    implicit object IntWitness extends JsonSerializable[Int]

    implicit object DoubleWitness extends JsonSerializable[Double]

    implicit object StringWitness extends JsonSerializable[String]

    implicit object ListWitness extends JsonSerializable[List[JsonSerializable[_]]]

    implicit object MapWitness extends JsonSerializable[Map[String, JsonSerializable[_]]]

}
