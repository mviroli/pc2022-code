package pc.utils

import org.scalatest.funsuite.AnyFunSuite

class StochasticsSpec extends AnyFunSuite{

  val choices = Set( 1.0->"a", 2.0->"b", 3.0->"C")

  test("Choices should correctly give cumulative list") {
    assertResult(
      List((1.0,"a"), (3.0,"b"), (6.0,"C"))
    )(
      Stochastics.cumulative(choices.toList)
    )
  }

  test("Choices should correctly draw") {
    val map = Stochastics.statistics(choices, 10000)

    assert(map("a")>1000)
    assert(map("a")<2000)
    assert(map("b")>2300)
    assert(map("b")<4300)
    assert(map("C")>4500)
    assert(map("C")<5500)
  }
}
