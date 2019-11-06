package pc.modelling

import java.util.Random

import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class StochasticChannelExperimentSpec extends FlatSpec{
  "CTMC analysis on ch" should "give right probability after 10000 runs" in {

    import pc.examples.StochasticChannel
    import pc.examples.StochasticChannel.state._

    val channel = StochasticChannel.stocChannel

    val channelAnalysis = CTMCAnalysis(channel)
    assert(channelAnalysis.experiment(10000,channelAnalysis.eventually(_==DONE),new Random(),IDLE,10)=== 0.998 +- 0.0019)
    assert(channelAnalysis.experiment(10000,channelAnalysis.eventually(_==DONE),new Random(),IDLE,10)!== 0.990 +- 0.0019)
  }
}
