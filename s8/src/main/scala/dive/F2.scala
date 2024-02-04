package dive

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Try
import java.text.ParseException
import scala.util.Failure
import scala.util.Success
import scala.concurrent.Future

object F2 {

  val o: Option[Int] = ???
  val e: Either[String, Int] = ???

  val t0: Either[Exception, String] = ???
  val t1: Try[String] = ???

  val t2: Try[Int] = Try {
    12 / 0
  }

  e match {
    case Left(value)  =>
    case Right(value) =>
  }

  t2 match {
    case Failure(exception) =>
    case Success(value)     =>

  }

  val f: Future[String] = ???

  f.map(str => str.length())

  f.onComplete {
    case Failure(exception) =>
    case Success(value)     =>
  }

}

object TryImpl {

  val o =
    try {

      // blah blah blah
    } catch {
      // case ParseException   =>
      // case RuntimeException =>
      case e =>
    }

  // Try2

  // Try2 {
  //   // computation
  // }

  val pert = Chert(12)

  object Chert {
    def apply(i: Int): Int = ???
  }
}
