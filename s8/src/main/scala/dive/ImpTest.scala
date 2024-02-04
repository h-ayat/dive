package dive

import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits.global

import scala.concurrent.duration._
import scala.concurrent.Await

case class PetId(value: String)
case class PersonId(value: String)
case class Person(id: PersonId, petId: PetId, name: String, lastName: String)
case class Pet(id: PetId, alive: Boolean)

object Db {

  // def getPerson(id: PersonId): Future[Option[Person]] = ???
  // def getPet(petId: PetId): Future[Option[Pet]] = ???

  def getPerson(id: PersonId): Future[Person] = ???
  def getPet(petId: PetId): Future[Pet] = ???

  def writePet(pet: Pet): Future[Unit] = ???
}

object ImpTest {
  // private implicit val ec: ExecutionContext = ???

  // def yekari(input: YeChizi):

  import Db._

  def killPet(pid: PersonId): Future[Unit] = {
    // getPerson(pid).flatMap(p => getPet(p.petId)).flatMap { pet =>
    //   writePet(pet.copy(alive = false))
    // }

    for {
      person <- getPerson(pid)
      pet <- getPet(person.petId)
      _ <- writePet(pet.copy(alive = false))
    } yield ()

  }

  def test(a: PersonId, b: PetId): Future[(Person, Pet)] = {

    val aa = getPerson(a)
    val bb = getPet(b)

    for {
      person <- aa
      pet <- bb
    } yield person -> pet
  }

  def getPersonPet(id: PersonId): Future[Pet] =
    for {
      person <- Db.getPerson(id)
      pet <- Db.getPet(person.petId)
    } yield pet

  def getPersonFullName(id: PersonId): Future[String] = {
    Db.getPerson(id).map(p => p.name + " " + p.lastName)
  }

  def test() {
    val id = PersonId("")

    val o = Db
      .getPerson(id)
      .map(p => p.name + " " + p.lastName)
      .map(_.size)
      .map(_ % 2 == 0)

    val result = Await.result(o, 2.seconds)
  }

}
