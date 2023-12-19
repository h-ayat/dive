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
}

case class Node[T](value: T, next: LL[T]) extends LL[T] {

  override def take(c: Int): LL[T] = ???

  override def drop(c: Int): LL[T] = ???

  override def takeWhile(p: T => Boolean): LL[T] = ???

  override def dropWhile(p: T => Boolean): LL[T] = ???

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

  override def take(c: Int): LL[T] = ???

  override def drop(c: Int): LL[T] = ???

  override def takeWhile(p: T => Boolean): LL[T] = ???

  override def dropWhile(p: T => Boolean): LL[T] = ???

  override def head: T = throw new Exception("")

  override def tail: LL[T] = this

  override def filter(p: T => Boolean): LL[T] = this

  override def size: Int = 0

  override def count(p: T => Boolean): Int = 0

}

object LL {

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

}

object Tester {

  val l = 1 :: 2 :: 3 :: Nil

  val myList = Node("b4", Node("aa5", Node("aaaaa6", Empty())))
  val test: List[String] = "b4" :: "aa5" :: "aaaaa6" :: Nil

  test.head
  test.tail
  

}
