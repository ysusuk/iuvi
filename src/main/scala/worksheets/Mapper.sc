import shapeless._
import shapeless.ops.hlist

trait ProductMapper[A, B, P] {
  def apply(a: A): B
}

implicit def genericProductMapper[A, B, P <: Poly, ARepr <: HList, BRepr <: HList](
  implicit
  poly: P,
  genA: Generic.Aux[A, ARepr],
  genB: Generic.Aux[B, BRepr],
  mapper: hlist.Mapper.Aux[P, ARepr, BRepr]
): ProductMapper[A, B, P] = new ProductMapper[A, B, P] {
  override apply(a: A): B = genB.from(mapper.apply(genA.to(a)))
}