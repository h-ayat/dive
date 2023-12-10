package dive

sealed trait LL

case class Node(value: String, next: LL) extends LL

case class Empty() extends LL

object LL {

  def filter(in: LL, p: String => Boolean): LL = in match {
    case Node(value, next) if p(value) =>
      Node(value, filter(next, p))
    case Node(value, next) =>
      filter(next, p)
    case Empty() =>
      Empty()
  }
  def exists(in: LL, p: String => Boolean): Boolean = in match {
    case Node(value, next) =>
      p(value) || exists(next, p)
    case Empty() =>
      false
  }

  def count(in: LL, p: String => Boolean): Int = in match {
    case Node(value, next) if p(value) =>
      1 + count(next, p)
    case Node(value, next) =>
      count(next, p)
    case Empty() =>
      0
  }
  def size(in: LL): Int = in match {
    case Node(value, next) =>
      1 + size(next)
    case Empty() =>
      0
  }

}
