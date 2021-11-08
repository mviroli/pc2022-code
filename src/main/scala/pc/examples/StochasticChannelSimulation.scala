package pc.examples

import pc.utils.Time
import pc.modelling.CTMCSimulation
import java.util.Random

object StochasticChannelSimulation extends App {

  import pc.examples.StochasticChannel.state._

  val channel = StochasticChannel.stocChannel

  val channelAnalysis = CTMCSimulation(channel)
  Time.timed{
    println(channelAnalysis.newSimulationTrace(IDLE, new Random)
                           .take(10)
                           .toList
                           .mkString("\n"))
  }
}