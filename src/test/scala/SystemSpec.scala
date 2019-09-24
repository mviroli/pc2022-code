import org.scalatest.FlatSpec
import pc.modelling.System

class SystemSpec extends FlatSpec{
  "A Channel" should "properly generate next states and paths" in {
    // Specification of my data-type for states
    object state extends Enumeration {
      val idle,send,done,fail = Value
    }
    type State = state.Value
    import state._

    // System specification
    val channel: System[State] = System.ofTransitions(
      idle->send,
      send->send,
      send->done,
      send->state.fail,
      state.fail->idle //,done->done
    )

    // Analysis, by testing via assertions
    assert(!channel.normalForm(idle))
    assert(channel.normalForm(done))

    assertResult(Set(send))(channel.next(idle));
    assertResult(Set(send,done,state.fail))(channel.next(send));

    assert(channel.paths(idle,3).contains(List(idle, send, send)));
    assert(channel.paths(idle,3).contains(List(idle, send, send)));
    assert(channel.completePaths(idle).contains(
           List(idle, send, send, send, state.fail, idle, send, done)));
  }
}
