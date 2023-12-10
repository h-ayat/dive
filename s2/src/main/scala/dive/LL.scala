package dive

sealed trait LL

case class Node(value: String, next: LL) extends LL

case class Empty() extends LL

object LL {

  def filter(in: LL, p: String => Boolean): LL = ???
  def exists(in: LL, p: String => Boolean): Boolean = ???
  def count(in: LL, p: String => Boolean): Int = ???
  def size(in: LL): Int = ???
}
