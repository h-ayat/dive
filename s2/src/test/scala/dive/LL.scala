package dive

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LLSpec extends AnyFlatSpec with Matchers {
  val myList = Node("b4", Node("aa5", Node("aaaaa6", Empty())))

  "LL" should "calculate size of a ll" in {
    LL.size(Empty()) shouldBe 0
    LL.size(myList) shouldBe 3
  }

  it should "filter" in {
    LL.size(LL.filter[String](myList, x => x.startsWith("a"))) shouldBe 2
    LL.size(LL.filter[String](myList, x => x.startsWith("b"))) shouldBe 1
    LL.size(LL.filter[String](myList, x => x.startsWith("c"))) shouldBe 0
    LL.size(LL.filter[String](Empty(), x => x.startsWith("c"))) shouldBe 0
  }

  it should "support exists " in {
    LL.exists[String](Empty(), x => x.size == 1) shouldBe false
    LL.exists[String](myList, x => x.size == 2) shouldBe true
    LL.exists[String](myList, x => x.size == 7) shouldBe false
  }

  it should "support count of" in {
    LL.count[String](Empty(), x => true) shouldBe 0
    LL.count[String](Empty(), x => false) shouldBe 0
    LL.count[String](myList, x => x.size > 2) shouldBe 2
    LL.count[String](myList, x => x.size >= 2) shouldBe 3
    LL.count[String](myList, x => x.size >= 99) shouldBe 0
  }

  it should "init on empty list" in {
    LL.init(Empty()) shouldBe Empty()
  }

  it should "init on single element list" in {
    LL.init(Node("a", Empty())) shouldBe Empty()

  }

  it should "init on more than one element" in {
    LL.init(myList) shouldBe Node("b4", Node("aa5", Empty()))
  }

}
