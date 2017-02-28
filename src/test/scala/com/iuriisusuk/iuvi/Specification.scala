package com.iuriisusuk.iuvi

import com.amazonaws.services.dynamodbv2.{document => dynamodb}
import org.scalacheck.Shapeless._
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Properties}

// TODO: generate and test arbitrary case class (will be easier by having decoder in place)
// then we can test relation decode(encode(Person)) == Person
case class Person(name: String, age: Int, married: Boolean)
object Item {
  def apply(name: String, age: Int, married: Boolean): dynamodb.Item = {
    val map = new java.util.HashMap[String, Object]()
    map.put("name", name.asInstanceOf[Object])
    map.put("age", age.asInstanceOf[Object])
    map.put("married", married.asInstanceOf[Object])

    dynamodb.Item.fromMap(map)
  }
}

object Specification extends Properties("ItemEncoder") {
  import ItemEncoder._

//  property("simple") = forAll { (x: String, y: Int, z: Boolean) =>
//    encode(Person(x, y, z)) == Item(x, y, z)
//  }

//  implicit val gen: Arbitrary[Person] = Arbitrary(for {
//    name <- arbitrary[String]
//    age <- arbitrary[Int]
//    married <- arbitrary[Boolean]
//  } yield Person(name, age, married))

  property("not sos simple") = forAll { person: Person =>
    encode(person) == Item(person.name, person.age, person.married)
  }

}
