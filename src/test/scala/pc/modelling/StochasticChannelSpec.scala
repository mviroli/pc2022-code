package pc.modelling;

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.*

class StochasticChannelSpec extends AnyFunSuite:

  import pc.examples.StochasticChannel.*

  test("Stochastic channel should correctly draw transitions") {
    stocChannel.transitions(IDLE) shouldBe Set(1.0->SEND)
    stocChannel.transitions(SEND) shouldBe Set(100000->SEND, 200000->DONE, 100000->FAIL)
    stocChannel.transitions(FAIL) shouldBe Set(100000->IDLE)
    stocChannel.transitions(DONE) shouldBe Set(1->DONE)
  }
