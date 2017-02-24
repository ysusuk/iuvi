import shapeless.{DepFn1, DepFn2}

import scala.collection.mutable

class Item(val map: mutable.Map[String, Any] = mutable.Map[String, Any]()) {
  def getString(attrName: String): String = map.get(attrName).get.asInstanceOf[String]
  def getInt(attrName: String): Int = map.get(attrName).get.asInstanceOf[Int]
  def getBoolean(attrName: String): Boolean = map.get(attrName).get.asInstanceOf[Boolean]
  //  def getMap(attrName: String): Map[String, String] = Map("attrName" -> "attrValue")

  def setString(attrName: String, value: String): Unit = map.put(attrName, value)
  def setInt(attrName: String, value: Int): Unit = map.put(attrName, value)
  def setBoolean(attrName: String, value: Boolean): Unit = map.put(attrName, value)

  override def toString() = map.toString()
}

trait ItemDecoder[A] extends DepFn1[Item] {
  type Out = A
}

