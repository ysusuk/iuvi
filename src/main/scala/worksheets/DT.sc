import shapeless.{Generic, DepFn1, HList, ::}
import shapeless.ops.hlist.Last

trait Second[L <: HList] { // extends DepFn1[L]
  type Out
  def apply(t: L): Out
}

object Second {
  type Aux[L <: HList, O] = Second[L] {type Out = O}

  def apply[L <: HList](implicit inst: Second[L]): Aux[L, inst.Out] =
    inst
}

implicit def hlistSecond[A, B, Rest <: HList]: Second.Aux[A :: B :: Rest, B] =
  new Second[A :: B :: Rest] {
    type Out = B
    def apply(value: A :: B :: Rest): B =
      value.tail.head
  }

def lastField[A, R <: HList](input: A)(
  implicit
  gen: Generic.Aux[A, R],
  last: Last[R]
): last.Out = last.apply(gen.to(input))