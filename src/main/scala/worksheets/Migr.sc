import shapeless.{Generic, HList, HNil}
import shapeless.ops.hlist.{Align, Intersection}
import shapeless.record._

case class IceCreamV1(name: String, numCherries: Int, inCone: Boolean)

case class IceCreamV2a(name: String, inCone: Boolean)
case class IceCreamV2b(name: String, inCone: Boolean, numCherries: Int)
case class IceCreamV2c(name: String, inCone: Boolean, numCherries: Int, numWaffles: Int)

trait Migration[A, B] {
  def apply(a: A): B
}

implicit class MigrationOps[A](a: A) {
  def migrateTo[B](implicit migration: Migration[A, B]): B = migration.apply(a)
}

implicit def genericMigration[A, B, ARepr <: HList, BRepr <: HList, Unaligned <: HList](
  implicit
  aGen: Generic.Aux[A, ARepr],
  bGen: Generic.Aux[B, BRepr],
  int: Intersection.Aux[ARepr, BRepr, Unaligned],
  align: Align[Unaligned, BRepr]
): Migration[A, B] = new Migration[A, B] {
  def apply(a: A): B = bGen.from(align.apply(int.apply(aGen.to(a))))
}

IceCreamV1("vb", 0, true).migrateTo[IceCreamV2a]
IceCreamV1("vb", 0, true).migrateTo[IceCreamV2b]
//IceCreamV1("Sundae", 1, true).migrateTo[IceCreamV2c]

("" :: HNil)