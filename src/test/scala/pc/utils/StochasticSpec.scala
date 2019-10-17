package pc.utils

import org.scalatest.FlatSpec

class StochasticSpec extends FlatSpec{

  val possibilities = Set( 1.0->"a", 2.0->"b", 3.0->"c")

  "Possibilities" should "correctly draw" in {
    val map =(1 to 10000).map(i => Stochastics.draw(possibilities))
                         .groupBy(identity)
                         .mapValues(_.size)

    assert(map("a")>1500)
    assert(map("a")<1700)
    assert(map("b")<3500)
    assert(map("b")>3000)
    assert(map("c")<5200)
    assert(map("c")>4800)
  }
}
