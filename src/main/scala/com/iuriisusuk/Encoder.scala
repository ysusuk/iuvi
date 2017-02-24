import com.amazonaws.services.dynamodbv2.document.Item
import shapeless.labelled.FieldType
import shapeless.{::, HList, HNil, LabelledGeneric, Witness, _}

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
    ItemEncoder.instance { value =>
      val item = new Item
      item.withString(witness.value.name, value)
      item
    }

  implicit def intEncoder[K <: Symbol, V <: Int](
    implicit witness: Witness.Aux[K]
  ): ItemEncoder[FieldType[K, V]] =
    ItemEncoder.instance { value =>
      val item = new Item
      item.withInt(witness.value.name, value)
      item
    }

  implicit def booleanEncoder[K <: Symbol, V <: Boolean](
    implicit witness: Witness.Aux[K]
  ): ItemEncoder[FieldType[K, V]] =
    ItemEncoder.instance { value =>
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
    ItemEncoder.instance { value =>
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
}


//case class Person(name: String, age: Int, married: Boolean)
//case class Maker(name: String)
//case class IceCream(name: String, subName: String, price: Int, maker: Maker)
//
//def encode[A](toEncode: A)(implicit itemEncoder: ItemEncoder[A]) =
//  itemEncoder.encode(toEncode)
//
//case class PersonWithIceCream(person: Person, iceCream: IceCream)
//val pGen = LabelledGeneric[Person].to(Person("Bob", 37, true))
//encode(pGen)
//encode(IceCream("engel", "blau", 1, Maker("de")))
//
//val personWithIceCream = new PersonWithIceCream(Person("bob", 37, true),
//  IceCream("engel", "balue", 1, Maker("de")))
//val pWithI = LabelledGeneric[PersonWithIceCream].to(personWithIceCream)
//encode(personWithIceCream)

//1 implement primitive types encoders
//2 implement product (hlist) type encoder based on 1
//3 create generic repr of case class and use 2 to
//3 Generic[CaseClass].to(CaseClass())
//  => Generic.Repr => Generic.Aux[A, Repr]



