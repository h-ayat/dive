package dive

import scala.collection.immutable

object Abstraction {




  def wordCount(l: List[String]): Int = {
    reduce(l.map(wordCount))(_ + _)
  }

  def wordCount(s: String): Int = s.count(_ == ' ') + 1

  def reduce[T](l: List[T])(f: (T, T) => T): T = l match {
    case head :: Nil =>
      head
    case head :: tail =>
      f(head, reduce(tail)(f))
    case immutable.Nil =>
      throw new Exception("Could not perform x on empty list")
  }

  def sum(l: List[String]): String =
    reduce(l)(_ + _)

  def sum(l: List[Int]): Int =
    reduce(l)(_ + _)

  def product(l: List[Int]): Int = reduce(l)(_ * _)

  def min[T](l: List[T])(compare: (T, T) => Boolean): T = {
    reduce(l) { (a, b) =>
      if (compare(a, b)) a else b
    }
  }

  def min(l: List[Int]): Int =
    reduce(l) { (a, b) =>
      if (a < b) a else b
    }

  def max(l: List[Int]): Int =
    reduce(l)({ (a, b) =>
      if (a > b) a else b
    })
}

final case class Person(name: String, age: Int, salary: Int)
