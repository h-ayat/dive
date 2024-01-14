package dive

import scala.collection.immutable

sealed trait LL[T] {
  def head: T
  def tail: LL[T]

  def filter(p: T => Boolean): LL[T]
  def size: Int
  def count(p: T => Boolean): Int

  def take(c: Int): LL[T]
  def drop(c: Int): LL[T]
  def takeWhile(p: T => Boolean): LL[T]
  def dropWhile(p: T => Boolean): LL[T]

  def map[U](f: T => U): LL[U]

}

case class Node[T](value: T, next: LL[T]) extends LL[T] {

  override def map[U](f: T => U): LL[U] = Node(f(value), next.map(f))

  override def take(c: Int): LL[T] =
    if (c <= 0) Empty[T]()
    else Node(value, next.take(c - 1))

  override def drop(c: Int): LL[T] = if (c <= 0) this
  else next.drop(c - 1)

  override def takeWhile(p: T => Boolean): LL[T] =
    if (p(value)) Node(value, next.takeWhile(p))
    else Empty()

  override def dropWhile(p: T => Boolean): LL[T] =
    if (p(value)) next.dropWhile(p)
    else this

  override def filter(p: T => Boolean): LL[T] = {
    val other = next.filter(p)

    if (p(value)) Node(value, other)
    else other
  }

  override def head: T = value

  override def tail: LL[T] = next

  override def size: Int = 1 + next.size

  override def count(p: T => Boolean): Int = {
    val base = if (p(value)) 1 else 0

    base + next.count(p)
  }

}

case class Empty[T]() extends LL[T] {

  override def map[U](f: T => U): LL[U] = Empty[U]()

  override def take(c: Int): LL[T] = this

  override def drop(c: Int): LL[T] = this

  override def takeWhile(p: T => Boolean): LL[T] = this

  override def dropWhile(p: T => Boolean): LL[T] = this

  override def head: T = throw new Exception("")

  override def tail: LL[T] = this

  override def filter(p: T => Boolean): LL[T] = this

  override def size: Int = 0

  override def count(p: T => Boolean): Int = 0

}

object LL {

  // Int => String
  // String => Double

  // Int => Double
  // Currying

  private def productDirect(x: Int, y: Int): Int = x * y
  val tempWithoutCurry: Int => Int = productDirect(_, 3)

  def testAmin(a: Int, s: String): Int = ???

  def triple(a: Int, b: String, c: Boolean): Unit = println(a + b + c)

  val resultTriple = triple(_, _, true)
  val resultTriple2 = resultTriple(_, "3")
  val finalResult = resultTriple2(4)

  private def productExternalCurry(x: Int): Int => Int = {
    // y => x * y
    def product_2(y: Int): Int = x * y
    product_2
  }
  private def productInternalCurry(x: Int)(y: Int): Int = x * y

  val temp: Int => Int = productInternalCurry(2)

  productDirect(2, 3)
  productExternalCurry(2)(3)

  val input = Node(1, Node(2, Empty()))

  private def add2(x: Int): Int = x + 2

  //   _ +  _
  private def add(x: Int, y: Int): Int = x + y

  //                              x => x + 2
  val result = map(input)(_ + 2)
  val result2 = map(input)(x => x * x + 2 * x)

  def map[T, U](in: LL[T])(f: T => U): LL[U] = in match {
    case Node(value, next) =>
      Node(f(value), map(next)(f))
    case Empty() => Empty()
  }

  def concat[T](before: LL[T], after: LL[T]): LL[T] = before match {
    case Node(value, next) =>
      Node(value, concat(next, after))
    case Empty() =>
      after
  }

  def flatten[T](in: LL[LL[T]]): LL[T] = in match {
    case Node(value, next) =>
      concat(value, flatten(next))

    case Empty() =>
      Empty()
  }

  def flatMap1[T, U](in: LL[T], f: T => LL[U]): LL[U] = {
    flatten(map(in)(f))
  }

  def flatMap2[T, U](in: LL[T], f: T => LL[U]): LL[U] = in match {
    case Node(value, next) =>
      concat(f(value), flatMap2(next, f))
    case Empty() =>
      Empty()
  }

  def head[T](in: LL[T]): T = in match {
    case Node(value, next) => value
    case Empty() =>
      throw new Exception("head on empty list")
  }

  def tail[T](in: LL[T]): LL[T] = in match {
    case Node(value, next) => next
    case Empty()           => Empty()
  }

  def last[T](in: LL[T]): T = in match {
    case Node(value, next) =>
      next match {
        case Node(v, n) =>
          last(next)
        case Empty() =>
          value
      }

    case Empty() =>
      throw new Exception("last on empty list")
  }

  def init[T](in: LL[T]): LL[T] = in match {
    case Node(value, next) =>
      next match {
        case Node(v, n) =>
          Node(value, init(next))
        case Empty() =>
          Empty()
      }
    case Empty() =>
      Empty()
  }

  def filter[T](in: LL[T], p: T => Boolean): LL[T] = in match {
    case Node(value, next) if p(value) =>
      Node(value, filter(next, p))
    case Node(value, next) =>
      filter(next, p)
    case Empty() =>
      Empty()
  }
  def exists[T](in: LL[T], p: T => Boolean): Boolean = in match {
    case Node(value, next) =>
      p(value) || exists(next, p)
    case Empty() =>
      false
  }

  def count[T](in: LL[T], p: T => Boolean): Int = in match {
    case Node(value, next) if p(value) =>
      1 + count(next, p)
    case Node(value, next) =>
      count(next, p)
    case Empty() =>
      0
  }
  def size[T](in: LL[T]): Int = in match {
    case Node(value, next) =>
      1 + size(next)
    case Empty() =>
      0
  }

  def max(in: LL[Int]): Int = ???

}

object Tester {


  // ArrayList<Cat> cats = ???
  // test(cats)
  // cats.foreach{ cat => cat.meow}

  // void test(ArrayList<Animal> animals) {
  //   Dog dog = new Dog();
  //   animals.add(dog);
  // }

  // Cat <: Animal
  // X[Cat] <: X[Animal]
  // X[Cat] >: X[Animal]

  val nestedList: LL[LL[Int]] =
    Node(5, Node(3, Node(1, Empty()))).map((x) => Node(x, Empty()))
  println(LL.flatten(nestedList))

  // val n: LL[Node[Int]] = ???
  // // test(n)

  // def test(in: LL[LL[Int]]): Int = ???

  // def flatten[T](in: LL[LL[T]]): LL[T] = ???
  //  in match {
  //   case Node(value, next) =>
  //     LL.concat(value, flatten(next))

  //   case Empty() =>
  //     Empty()
  // }
}
