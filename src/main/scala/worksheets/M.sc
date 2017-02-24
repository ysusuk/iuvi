import shapeless.labelled.FieldType
import shapeless.ops.record.ToMap
import shapeless.{::, Generic, HList, HNil, LabelledGeneric, Witness}

implicit val hnilToMapRec: ToMap[HNil] = new ToMap[HNil] {
  def apply(l: HNil): Map[String, Any] = Map.empty
}

implicit def hconsToMapRec0[K <: Symbol, V, R <: HList, T <: HList](implicit
  wit: Witness.Aux[K],
  gen: LabelledGeneric.Aux[V, R],
  tmrH: ToMap[R],
  tmrT: ToMap[T]
): ToMap[FieldType[K, V] :: T] = new ToMap[FieldType[K, V] :: T] {
  def apply(l: FieldType[K, V] :: T): Map[String, Any] =
    tmrT(l.tail) + (wit.value.name -> tmrH(gen.to(l.head)))
}