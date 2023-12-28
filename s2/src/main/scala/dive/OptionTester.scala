package dive

object OptionTester {

  def getPersonsSensor(id: String): Option[Int] = {

    for {
      person <- findPerson(id)
      model <- person.phoneModel
      phone <- findPhone(model)
      sensorModel <- phone.sensorModel
      sensor <- findSensor(sensorModel)
    } yield {
      sensor.pixels
    }

    // findPerson(id).flatMap{ person =>
    //   person.phoneModel.flatMap{ model =>

    //     }
    //   }
    // // findPerson(id) match {
    //   case None =>
    //     None
    //   case Some(p) =>
    //     p.phoneModel match {
    //       case None => None
    //       case Some(phoneModel) =>
    //         findPhone(phoneModel) match {
    //           case None =>
    //             None
    //           case Some(phone) =>

    //         }

    //     }

  }

  def findPerson(id: String): Option[Person] = ???
  def findPhone(id: String): Option[Phone] = ???
  def findSensor(id: String): Option[Sensor] = ???

  case class Person(name: String, phoneModel: Option[String])
  case class Phone(model: String, sensorModel: Option[String])
  case class Sensor(model: String, pixels: Int)
}
