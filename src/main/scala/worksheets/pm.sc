import shapeless.{:+:, CNil, Inl, Inr}

case class Red()
case class Amber()
case class Green()
type Light = Red :+: Amber :+: Green :+: CNil

def f(l: Light) = l match {
   case Inl(Red()) => 0
   case Inr(Inl(Amber())) => 1
   case Inr(Inr(Inl(Green()))) => 2
   case _ => 3
}

f(Inl(Red()))