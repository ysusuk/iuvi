import shapeless.labelled.FieldType
import shapeless.{HList, HNil, ::, Lazy, Witness}

sealed trait JsonValue
case class JsonArray(items: List[JsonValue]) extends JsonValue
case class JsonString(value: String) extends JsonValue
case class JsonNumber(value: Double) extends JsonValue
case class JsonBoolean(value: Boolean) extends JsonValue
case object JsonNull extends JsonValue
// map?
case class JsonObject(fields: List[(String, JsonValue)]) extends JsonValue

trait JsonEncoder[A] {
  def encode(value: A): JsonValue
}

object JsonEncoder {
  def apply[A](implicit encoder: JsonEncoder[A]): JsonEncoder[A] = encoder

  def instance[A](f: A => JsonValue): JsonEncoder[A] =
    new JsonEncoder[A] {
      override def encode(value: A) = f(value)

    }
}

implicit val stringEncoder: JsonEncoder[String] =
  JsonEncoder.instance(str => JsonString(str))
implicit val doubleEncoder: JsonEncoder[Double] =
  JsonEncoder.instance(num => JsonNumber(num))
implicit val intEncoder: JsonEncoder[Int] =
  JsonEncoder.instance(num => JsonNumber(num))
implicit val booleanEncoder: JsonEncoder[Boolean] =
  JsonEncoder.instance(bool => JsonBoolean(bool))
implicit def listEncoder[A](implicit encoder: JsonEncoder[A]): JsonEncoder[List[A]] =
  JsonEncoder.instance(list => JsonArray(list.map(encoder.encode)))
implicit def optionEncoder[A](implicit encoder: JsonEncoder[A]): JsonEncoder[Option[A]] =
  JsonEncoder.instance(opt => opt.map(encoder.encode).getOrElse(JsonNull))

trait JsonObjectEncoder[A] extends JsonEncoder[A] {
  def encode(value: A): JsonObject
}

object JsonObjectEncoder {
  def instance[A](f: A => JsonObject): JsonObjectEncoder[A] =
    new JsonObjectEncoder[A] {
      override def encode(value: A): JsonObject = f(value)
    }
}

val hnilEncoder: JsonObjectEncoder[HNil] = JsonObjectEncoder.instance(hnil => JsonObject(Nil))

implicit def hlistEncoder[K <: Symbol, H, T <: HList](
  implicit
  witness: Witness.Aux[K],
  hEncoder: Lazy[JsonEncoder[H]],
  tEncoder: JsonObjectEncoder[T]
): JsonObjectEncoder[FieldType[K, H] :: T] = {
  val fieldName = witness.value.name
  JsonObjectEncoder.instance {
    hlist =>
      val head = hEncoder.value.encode(hlist.head)
      val tail = tEncoder.encode(hlist.tail)
      JsonObject((fieldName, head) :: tail.fields)
  }

}