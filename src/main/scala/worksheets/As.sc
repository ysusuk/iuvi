//import $ivy.`com.chuusai::shapeless:2.3.2`
//
//import shapeless._


  import shapeless.{Generic, HList, ::, HNil, Coproduct, :+:, CNil, Inl, Inr}

  trait CsvEncoder[A] {
    def encode(value: A): List[String]
  }

  object CsvEncoder {

    // summoner
    def apply[A](implicit enc: CsvEncoder[A]): CsvEncoder[A] =
      enc

    // pure, reduces boilerplate of anonymous classes
    def instance[A](func: A => List[String]): CsvEncoder[A] =
      new CsvEncoder[A] {
        def encode(value: A): List[String] =
          func(value)
      }
  }

  case class Employee(name: String, number: Int, manager: Boolean)
  case class IceCream(name: String, number: Int, manager: Boolean)

  // rule, how to encode primitive
  implicit val stringEncoder: CsvEncoder[String] =
    CsvEncoder.instance(str => List(str))

  implicit val intEncoder: CsvEncoder[Int] =
    CsvEncoder.instance(i => List(i.toString))

  implicit val boolEncoder: CsvEncoder[Boolean] =
    CsvEncoder.instance(b => List(if (b) "yes" else "no"))

  implicit val hnilEncoder: CsvEncoder[HNil] =
    CsvEncoder.instance(_ => Nil)

  // rule, how to encode hlist - product
  implicit def hlistEncoder[H, T <: HList](
      implicit
      hEncoder: CsvEncoder[H],
      tEncoder: CsvEncoder[T]
  ): CsvEncoder[H :: T] =
    CsvEncoder.instance {
      // encode traverses one layer only, unlike the common recursive map function - *the non-recursive map trick
      case h :: t =>
        hEncoder.encode(h) ++ tEncoder.encode(t)
    }

//  implicit val cnilEncoder: CsvEncoder[CNil] =
//    CsvEncoder.instance(_ => Nil)
//
//  implicit def clistEncoder[H, T <: Coproduct](
//      implicit hEncoder: CsvEncoder[H],
//      tEncoder: CsvEncoder[T]
//  ): CsvEncoder[H :+: T] =
//    CsvEncoder.instance {
//      case Inl(h) => hEncoder.encode(h)
//      case Inr(t) => tEncoder.encode(t)
    }

  //  implicit val employeeEncoder: CsvEncoder[Employee] =
  //    CsvEncoder.instance { employee =>
  //      List(
  //        employee.name,
  //        employee.number.toString,
  //        if (employee.manager) "yes" else "no"
  //      )
  //    }

  //  implicit def pairEncoder[A, B](
  //      implicit aEncoder: CsvEncoder[A],
  //      bEncoder: CsvEncoder[B]
  //  ): CsvEncoder[(A, B)] =
  //    new CsvEncoder[(A, B)] {
  //      def encode(pair: (A, B)): List[String] = {
  //        val (a, b) = pair
  //        aEncoder.encode(a) ++ bEncoder.encode(b)
  //      }
  //    }

  //  implicit val iceCreamEncoder: CsvEncoder[IceCream] = {
  //    val gen = Generic[IceCream]
  //    val encoder = CsvEncoder[gen.Repr]
  //    CsvEncoder.instance(iceCream => encoder.encode(gen.to(iceCream)))
  //  }

  implicit def genericEncoder[A, R](
      implicit
      gen: Generic.Aux[A, R],
      encoder: CsvEncoder[R]
  ): CsvEncoder[A] =
    CsvEncoder.instance(a => encoder.encode(gen.to(a)))

  def writeCsv[A](values: List[A])(implicit encoder: CsvEncoder[A]) =
    values.map(encoder.encode(_).mkString(",")).mkString("\n")


