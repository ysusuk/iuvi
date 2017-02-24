class Item(id: String) {
  def getString(attrName: String): String = s"$id-strValue"
  def getInt(attrName: String): Int = 0
  def getBoolean(attrName: String): Boolean = true
  def getMap(attrName: String): Map[String, String] = Map("attrName" -> "attrValue")
}

trait ItemDecoder[A] {
  def decode(value: (Item, String)): A
}

object ItemDecoder {
  def apply[A](implicit decoder: ItemDecoder[A]): ItemDecoder[A] = decoder

  def instance[A](f: ((Item, String)) => A): ItemDecoder[A] =
    new ItemDecoder[A] {
      override def decode(value: (Item, String)) = f(value)
    }

}

import shapeless.{::, HList, _}

implicit val stringDecoder: ItemDecoder[String] =
  ItemDecoder.instance {
    case (item, attrName) => item.getString(attrName)
  }

implicit val intDecoder: ItemDecoder[Int] =
  ItemDecoder.instance {
    case (item, attrName) => item.getInt(attrName)
  }

implicit val booleanDecoder: ItemDecoder[Boolean] =
  ItemDecoder.instance {
    case (item, attrName) => item.getBoolean(attrName)
  }

implicit def hlistEncoder[H, T <: HList](
  implicit
  item: Item,
  hEncoder: ItemDecoder[H],
  tEncoder: ItemDecoder[T]
): ItemDecoder[H :: T] =
  ItemDecoder.instance {
    case h :: t =>
      hEncoder.decode(h) ++ tEncoder.decode(t)
  }

case class EmployeeItem(name: String, age: Int, manager: Boolean)

def decode[A](item: Item, attrName: String)(implicit itemDecoder: ItemDecoder[A]) =
  itemDecoder.decode(item, attrName)

val i = new Item("0")

// ??? how to get type and attr name
decode[String](i, "")
decode[Int](i, "")
decode[Boolean](i, "")

// LabelledGeneric[EmployeeItem].to(EmployeeItem("Bob", 37, true))

import shapeless.ops.hlist
