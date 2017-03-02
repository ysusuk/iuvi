package com.iuriisusuk.iuvi

import com.amazonaws.services.dynamodbv2.document.Item
import shapeless.labelled.FieldType
import shapeless.{::, HList, HNil, LabelledGeneric, Witness, _}

//1 implement primitive types encoders
//2 implement product (hlist) type encoder based on 1
//3 create generic repr of case class and use 2 to
//3 Generic[CaseClass].to(CaseClass())
//  => Generic.Repr => Generic.Aux[A, Repr]

// is A because can be String, Int or HList
trait ItemEncoder[A] {
  def encode(value: A): Item
}

object ItemEncoder {
  def apply[A](implicit encoder: ItemEncoder[A]): ItemEncoder[A] = encoder

  def instance[A](f: A => Item): ItemEncoder[A] =
    new ItemEncoder[A] {
      override def encode(value: A): Item = f(value)
    }

  implicit def stringEncoder[K <: Symbol, V <: String](
    implicit witness: Witness.Aux[K]
  ): ItemEncoder[FieldType[K, V]] =
    instance { value =>
      val item = new Item
      item.withString(witness.value.name, value)
      item
    }

  implicit def intEncoder[K <: Symbol, V <: Int](
    implicit witness: Witness.Aux[K]
  ): ItemEncoder[FieldType[K, V]] =
    instance { value =>
      val item = new Item
      item.withInt(witness.value.name, value)
      item
    }

  implicit def booleanEncoder[K <: Symbol, V <: Boolean](
    implicit witness: Witness.Aux[K]
  ): ItemEncoder[FieldType[K, V]] =
    instance { value =>
      val item = new Item
      item.withBoolean(witness.value.name, value)
      item
    }

  // K is key, A is value, R is HList representation of A
  implicit def nestedClassEncoder[K <: Symbol, A, R](
    implicit
    witness: Witness.Aux[K],
    generic: LabelledGeneric.Aux[A, R],
    encoder: ItemEncoder[R]
  ): ItemEncoder[FieldType[K, A]] =
    instance { value =>
      val i = encoder.encode(generic.to(value))
      val item = new Item
      val m = new java.util.HashMap[String, Any]()
      item.withMap(witness.value.name, i.asMap())
      item
    }

  import cats.Monoid

  implicit val itemMonoid: Monoid[Item] = new Monoid[Item] {
    override def empty: Item = new Item()

    override def combine(x: Item, y: Item): Item = {
      val m = x.asMap
      m.putAll(y.asMap())
      Item.fromMap(m)
    }
  }

  implicit val hnilEncoder: ItemEncoder[HNil] =
    ItemEncoder.instance(_ => new Item())

  implicit def hlistEncoder[H, T <: HList, R](
    implicit
    hEncoder: Lazy[ItemEncoder[H]],
    tEncoder: ItemEncoder[T],
    monoid: Monoid[Item]
  ): ItemEncoder[H :: T] =
    ItemEncoder.instance { value =>
      //      println("hlist enc")
      val itemX = hEncoder.value.encode(value.head)
      val itemY = tEncoder.encode(value.tail)
      monoid.combine(itemX, itemY)
    }

  implicit def genericEncoder[A, R](
    implicit
    generic: LabelledGeneric.Aux[A, R],
    itemEncoder: Lazy[ItemEncoder[R]]
  ): ItemEncoder[A] =
    ItemEncoder.instance { value =>
      //      println("gen enc")
      itemEncoder.value.encode(generic.to(value))
    }

  def encode[A](toEncode: A)(implicit itemEncoder: ItemEncoder[A]) =
    itemEncoder.encode(toEncode)
}



