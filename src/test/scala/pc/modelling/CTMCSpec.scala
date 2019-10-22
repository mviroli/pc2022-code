package pc.modelling;

import org.scalatest.FlatSpec

class CTMCSpec extends FlatSpec{
  "CTMC ch" should "work as expected" in {
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

    assert(channel.transitions(idle) == Set(1.0->send))
    assert(channel.transitions(send) == Set(100000->send, 200000->done, 100000->fail))
    assert(channel.transitions(fail) == Set(100000->idle))
    assert(channel.transitions(done) == Set(1->done))
  }
}
