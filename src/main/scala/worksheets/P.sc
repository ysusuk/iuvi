import shapeless._

object p extends Poly1 {

  implicit val intCase: Case.Aux[Int, Double] = at (num => num / 2.0)

  implicit val stringCase: Case.Aux[String, Int] =
    at(str => str.length)
}

p(1)
p("blah")