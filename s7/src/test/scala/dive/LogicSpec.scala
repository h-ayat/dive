package dive

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LogicSpec extends AnyFlatSpec with Matchers {

  "Min function" should "find minimum" in {
    import Logic.min

    val gen: Gen[List[Int]] = Gen.list(Gen.positiveInt, 1000)

    for {
      _ <- 1 to 1000
    } {
      val list = gen.next
      val m = min(list)
      list.forall(m <= _)
    }
  }

  it should "generate random people" in {

    val personGenerator = for {
      age <- Gen.lessThanInt(120)
      name <- Gen.chertString
    } yield Person(name, age)

  }
}
