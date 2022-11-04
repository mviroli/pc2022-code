package pc.utils

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.*

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

    assert(map("a")>1600)
    assert(map("a")<1720)
    assert(map("b")>3000)
    assert(map("b")<3500)
    assert(map("C")>4800)
    assert(map("C")<5200)
  }
}
