package dive

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AbstractionTest extends AnyFlatSpec with Matchers {

  import Abstraction.min

  "min" should "find minimum" in {
    min(1 :: Nil) shouldBe 1
    min(1 :: 1 :: Nil) shouldBe 1
    min(1 :: 2 :: Nil) shouldBe 1
    min(2 :: 1 :: Nil) shouldBe 1
  }

  it should "work on person" in {
    val p1 = Person("a", 67, 100)
    val p2 = Person("b", 17, 200)
    val p3 = Person("c", 67, 200)

    val l = List(p1, p2, p3)

    min(l)(_.name > _.name) shouldBe p3
    min(l)(_.name < _.name) shouldBe p1
    min(l)(_.age > _.age) shouldBe p3
    min(l)(_.salary > _.salary) shouldBe p3

    l.sortBy(_.age)

  }

  "max" should "find maximum" in {
    Abstraction.max(1 :: Nil) shouldBe 1
    Abstraction.max(1 :: 1 :: Nil) shouldBe 1
    Abstraction.max(1 :: 2 :: Nil) shouldBe 2
    Abstraction.max(2 :: 1 :: Nil) shouldBe 2

  }

  "sum" should "sum all numbers" in {
    Abstraction.sum(0 :: Nil) shouldBe 0
    Abstraction.sum(1 :: Nil) shouldBe 1
    Abstraction.sum(1 :: 1 :: Nil) shouldBe 2
    Abstraction.sum(1 :: 2 :: Nil) shouldBe 3
    Abstraction.sum(2 :: 1 :: Nil) shouldBe 3
  }

  it should "work on strings as well" in {
    import Abstraction.sum

    sum("a" :: Nil) shouldBe "a"
    sum("a" :: "b" :: Nil) shouldBe "ab"
  }

  "product" should "multiply all numbers" in {
    Abstraction.product(0 :: Nil) shouldBe 0
    Abstraction.product(1 :: Nil) shouldBe 1
    Abstraction.product(1 :: 1 :: Nil) shouldBe 1
    Abstraction.product(1 :: 2 :: Nil) shouldBe 2
    Abstraction.product(2 :: 1 :: Nil) shouldBe 2
    Abstraction.product((1 to 4).toList) shouldBe 24
  }

}
