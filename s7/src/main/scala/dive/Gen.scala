package dive

import scala.util.Random

trait Gen[T] {
  self =>

  def next: T

  def map[U](f: T => U): Gen[U] = new Gen[U] {
    override def next: U = f(self.next)
  }

  def flatMap[U](f: T => Gen[U]): Gen[U] = new Gen[U] {
    override def next: U = f(self.next).next
  }
}

object Gen {
  private val rand = new Random()

  def const[T](t: T) = new Gen[T] {
    override def next: T = t
  }

  val int = new Gen[Int] {
    override def next: Int = rand.nextInt
  }

  val positiveInt = int.map(Math.abs)

  def lessThanInt(max: Int) = positiveInt.map(_ % max)

  def list[T](gen: Gen[T], maxSize: Int = 10000): Gen[List[T]] =
    lessThanInt(maxSize)
      .map(_ + 1)
      .map(size => (0 to size).map(_ => gen.next).toList)

  val charGen = lessThanInt(256).map(_.toChar)

  def chertString(maxLength: Int): Gen[String] =
    list(charGen, maxLength).map(_.mkString)

  def oneOf[T](l: List[T]): Gen[T] = lessThanInt(l.size).map(l)

  def stringOf(in: String, maxLength: Int): Gen[String] = ???
}
