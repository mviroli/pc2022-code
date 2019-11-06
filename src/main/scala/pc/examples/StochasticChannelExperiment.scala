package pc.examples

import pc.utils.Time
import pc.modelling.CTMCAnalysis

object StochasticChannelExperiment extends App {

  import pc.examples.StochasticChannel.state._

  val channel = StochasticChannel.stocChannel

  val channelAnalysis = CTMCAnalysis(channel)
  val data = for (t <- (0.1 to 10.0 by 0.1).toStream;
                  p = channelAnalysis.experiment(
                        runs = 10000,
                        prop = channelAnalysis.eventually(_ == DONE),
                        s0 = IDLE,
                        timeBound = t)) yield (t, p)

  Time.timed{ println(data.mkString("\n")) }
  scalax.chart.api.XYLineChart(data).show() // with dependencies on scala-chart
}