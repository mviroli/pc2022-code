import java.util.Random

import examples.TryCTMCSimulation.State.Value
import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import pc.modelling.{CTMC, CTMCAnalysis}

class CTMCAnalysisSpec extends FlatSpec{
  "CTMC analysis on channel" should "give right probability" in {
    object State extends Enumeration {
      val idle,send,done,fail = Value
    }
    import State._
    val fail = State.fail

    val channel: CTMC[State.Value] = CTMC.ofTransitions(
      (idle,1.0,send),
      (send,100000.0,send),
      (send,200000.0,done),
      (send,100000.0,fail),
      (fail,100000.0,idle),
      (done,1.0,done)
    )

    val channelAnalysis = CTMCAnalysis(channel)
    assert(channelAnalysis.experiment(10000,channelAnalysis.eventually(_==done),new Random(),idle,10)=== 0.998 +- 0.0019)
  }
}
