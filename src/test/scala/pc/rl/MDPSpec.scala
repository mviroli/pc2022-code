package pc.rl

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.*
import pc.rl.examples.TwoWaysMDP
import pc.rl.examples.TwoWaysMDP.Action.*
import pc.rl.examples.TwoWaysMDP.State

class QFunctionSpec extends AnyFunSuite:

  test("QF should properly get"){
    assertResult( 1.0)( TwoWaysMDP.rl.qfTW()(State(0),left))
    assertResult( 0.0)( TwoWaysMDP.rl.qfTW()(State(20),left))
  }

  test("QF should properly update") {
    assertResult( 100)( TwoWaysMDP.rl.qfTW().update(State(0),left,100)(State(0),left))
    assertResult( 0.0)( TwoWaysMDP.rl.qfTW().update(State(10),left,100)(State(10),left))
  }

class MDPSpec extends AnyFunSuite:

  test("MDP should properly take") {
    assertResult((-0.2,State(5)))( TwoWaysMDP.rl.mdpTW().apply(State(4),right))
    assertResult((10.0,State(-5)))( TwoWaysMDP.rl.mdpTW().apply(State(-4),left))
  }

class RLSpec extends AnyFunSuite:

  val rl = TwoWaysMDP.rl.rlTW()

  test("QRL should properly updated QF") {
    var qf = TwoWaysMDP.rl.qfTW()
    qf = rl.updateQ(rl.system.initial,qf)._2
    assertResult(0.85)(qf(State(0),left))
  }

  test("QRL should properly run an episode") {
    var qf = TwoWaysMDP.rl.qfTW()
    qf = rl.learn(100,100,qf)
    assertResult(0.0)(qf(State(10),left))
    assertResult(10.0)(qf(State(-4),left))
    assert( (-5 to 10).forall(i => qf.bestPolicy(State(i))==left))
  }

  test("QRL should properly execute A run after learning") {
    var qf = TwoWaysMDP.rl.qfTW()
    qf = rl.learn(100,100,qf)
    assertResult( List((left,State(-1)), (left,State(-2)), (left,State(-3)), (left,State(-4)), (left,State(-5))) )(rl.system.run(qf.bestPolicy).toList)
  }
