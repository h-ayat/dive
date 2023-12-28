package dive

object ListTester {

  val oldL = Node(1, Node(2, Node(3, Empty())))
  val newL = Node(0, oldL)

  val l = List(1, 2, 3) // 1 :: 2 :: 3 :: Nil

  val lP2 = l.map(x => x.toString + " $")

  val l2 = 0 :: l // Node(0, l)

  val strs = "Ali" :: "Amir" :: Nil
  //         List(a, a ,a ) , List(a,a,a,a)
  "Ali".map(x => 'a')

  val ooo = strs.flatMap(str => str.map(x => 'a').toList)

  def append[T](in: LL[T], t: T): LL[T] = {
    in match {
      case Empty() => Node(t, Empty())
      case Node(value, next) =>
        Node(value, append(next, t))
    }
  }

  val badO: Int = ???
  val o: Option[Int] = ???

  def hamash(l: List[Person]): Int = {
    l.flatMap(p => p.phones.flatMap(phone => phone.price)).sum
  }

  def hamash2(companies: List[Company]): Int = {
    // List[A] + (A => List[T])
    // -> List[Option[T]] + flatten   -> List[T]

    val m: Map[String, Person] = ???

    val r1 = companies
      .flatMap(c => c.hr.flatMap(p => p.phones.map(phone => phone.price)))

    val r2 = for {
      c <- companies
      p <- c.hr
      phone <- p.phones
      price <- phone.price
    } yield price

    ???
  }
  case class Company(hr: List[Person])
  case class Person(name: String, phones: List[Phone])
  case class Phone(price: Option[Int])

}
