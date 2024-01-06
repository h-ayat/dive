package dive

object ADT {

  case class Address(city: String, street: String, number: Int)
  case class Person(name: String, age: Int, address: Address, pet: Animal)

  sealed trait Animal // 256 + 2
  case class Dog(name: Char) extends Animal // 256
  case class Cat(livesLeft: Boolean) extends Animal // 2


  case class Animal2(dog: Dog, cat: Cat) // 256 * 2

}
