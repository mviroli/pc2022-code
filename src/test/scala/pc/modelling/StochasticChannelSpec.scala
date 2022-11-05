package pc.modelling;

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.*

class StochasticChannelSpec extends AnyFunSuite{

  import pc.examples.StochasticChannel
  import pc.examples.StochasticChannel.state.*

  def ch = StochasticChannel.stocChannel

  test("Stochastic channel should correctly draw transitions") {
    ch.transitions(IDLE) shouldBe Set(1.0->SEND)
    ch.transitions(SEND) shouldBe Set(100000->SEND, 200000->DONE, 100000->FAIL)
    ch.transitions(FAIL) shouldBe Set(100000->IDLE)
    ch.transitions(DONE) shouldBe Set(1->DONE)
  }
}
