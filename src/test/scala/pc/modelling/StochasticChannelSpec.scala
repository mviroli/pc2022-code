package pc.modelling;

import org.scalatest.funsuite.AnyFunSuite

class StochasticChannelSpec extends AnyFunSuite{

  import pc.examples.StochasticChannel, pc.examples.StochasticChannel.state._

  def ch = StochasticChannel.stocChannel

  test("Stochastic channel should correctly draw transitions") {
    assertResult(Set(1.0->SEND))(ch.transitions(IDLE))
    assertResult(Set(100000->SEND, 200000->DONE, 100000->FAIL))(ch.transitions(SEND))
    assertResult(Set(100000->IDLE))(ch.transitions(FAIL))
    assertResult(Set(1->DONE))(ch.transitions(DONE))
  }
}
