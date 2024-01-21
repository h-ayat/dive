package dive

import scala.collection.immutable

object Logic {

  def min(in: List[Int]): Int = in match {
    case head :: Nil =>
      head
    case head :: next =>
      val nextMin = min(next)
      if (head <= nextMin) head else nextMin
    case immutable.Nil =>
      throw new Exception("Cannot calculate min on empty list")
  }
}


case class Person(name: String, age: Int)