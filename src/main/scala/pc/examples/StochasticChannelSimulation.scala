package pc.examples

import pc.utils.Time
import java.util.Random
import pc.examples.StochasticChannel.*

@main def mainStochasticChannelSimulation =
  Time.timed(
    println(stocChannel.newSimulationTrace(IDLE, new Random)
                           .take(10)
                           .toList
                           .mkString("\n")))