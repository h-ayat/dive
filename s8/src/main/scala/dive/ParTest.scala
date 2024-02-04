package dive

import scala.collection.parallel.CollectionConverters._

object ParTest {

  val samples = 100_000_000

  val data = (1 to samples).map(_.toLong * 900000L).toList
  val parData = data.par

  private def wrap(f: () => Int): Unit = {
    Thread.sleep(1000)

    val start = System.currentTimeMillis()
    val result = f()
    val duration = System.currentTimeMillis() - start
    println("result: " + result)
    println(duration)
  }

  def isPrime(n: Long): Boolean =
    (2 to Math.abs(math.sqrt(Math.abs(n)).toInt)) forall (x => n % x != 0)

  def test1(): Unit = {
    wrap { () =>
      data
        .map(isPrime)
        .indexOf(true)
    }
  }

  def test2(): Unit = {
    wrap { () =>
      parData
        .map(isPrime)
        .indexOf(true)
    }
  }
}
