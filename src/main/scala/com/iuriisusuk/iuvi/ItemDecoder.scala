package com.iuriisusuk.iuvi

import com.amazonaws.services.dynamodbv2.document.Item

trait ItemDecoder[A] {
  def decode(item: Item): A
}

object ItemDecoder {
  def apply[A](implicit decoder: ItemDecoder[A]): ItemDecoder[A] = decoder

  def instance[A](f: Item => A): ItemDecoder[A] =
    new ItemDecoder[A] {
      override def decode(item: Item) = f(item)
    }

  implicit val stringDecoder: ItemDecoder[String] =
    instance { item =>
      item.getString("")
    }
}
