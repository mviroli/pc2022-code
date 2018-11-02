package pc.modelling;

import org.scalatest.FlatSpec

class CTMCSpec extends FlatSpec{
  "CTMC channel" should "work as expected" in {
    object State extends Enumeration {
      val idle, send, done, fail = Value
    }
    import State._
    type State = State.Value
    val fail = State.fail

    val channel: CTMC[State] = CTMC.ofTransitions(
      (idle, 1.0, send),
      (send, 100000.0, send),
      (send, 200000.0, done),
      (send, 100000.0, fail),
      (fail, 100000.0, idle),
      (done, 1.0, done)
    )

    assert(channel.nextWithRate(idle) == Set(1.0->send))
    assert(channel.nextWithRate(send) == Set(100000->send, 200000->done, 100000->fail))
    assert(channel.nextWithRate(fail) == Set(100000->idle))
    assert(channel.nextWithRate(done) == Set(1->done))
  }
}
