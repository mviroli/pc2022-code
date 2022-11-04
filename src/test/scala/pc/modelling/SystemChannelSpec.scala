package pc.modelling

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.*

class SystemChannelSpec extends AnyFunSuite{
  import pc.examples.SystemChannel, pc.examples.SystemChannel.state._

  val ch = SystemChannel.channel;

  test("System Channel should properly identify normal forms"){
    assert(!ch.normalForm(IDLE))
    assert(ch.normalForm(DONE))
  }

  test("System Channel should properly draw next states"){
    assertResult(Set(SEND))(ch.next(IDLE));
    assertResult(Set(SEND, DONE, FAIL))(ch.next(SEND));
  }

  test("System Channel should properly generate paths"){
    assert(ch.paths(IDLE,3).contains(List(IDLE, SEND, SEND)));

    assertResult(
      List(List(IDLE, SEND, DONE), List(IDLE, SEND, SEND, DONE))
    )(
      ch.completePathsUpToDepth(IDLE,4).toList
    )

    assert(ch.completePaths(IDLE).contains(
           List(IDLE, SEND, SEND, SEND, FAIL, IDLE, SEND, DONE)));
  }
}
